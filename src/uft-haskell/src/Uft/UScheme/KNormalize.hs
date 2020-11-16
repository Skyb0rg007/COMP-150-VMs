
module Uft.UScheme.KNormalize
    ( module Uft.UScheme.KNormalize
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable        (foldl')
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.List            ((\\))
import           Data.Text            (Text)
import           Type.OpenADT
import           Uft.KNormal.Types
import           Uft.UScheme.Types
import           Uft.VScheme.Types
import           Uft.Util
import           Uft.Primitives

type FO = UScheme ++ '[KNCapturedF, KNClosureF Text, KNLetRecF Text]

newtype RegSet = RS Int

infix 6 -:-

(-:-) :: RegSet -> Int -> RegSet
RS n -:- r = RS (max n (succ r))

smallest :: RegSet -> Int
smallest (RS n) = n

bindAnyReg
    :: forall r m. ('[KNLetF Int, EVarLocalF Int] :<: r, Monad m)
    => RegSet
    -> OpenADT r
    -> (Int -> m (OpenADT r))
    -> m (OpenADT r)
bindAnyReg a (EVarLocal n) k = k n
bindAnyReg a e k = bindSmallest a e k

bindSmallest
    :: forall r m. (KNLetF Int :< r, Monad m)
    => RegSet
    -> OpenADT r
    -> (Int -> m (OpenADT r))
    -> m (OpenADT r)
bindSmallest a e k =
    let r = smallest a
     in KNLet r e <$> k r

nbRegsWith
    :: Monad m
    => (RegSet -> a -> m (OpenADT r))
    -> (RegSet -> OpenADT r -> (Int -> m (OpenADT r)) -> m (OpenADT r))
    -> RegSet
    -> [a]
    -> ([Int] -> m (OpenADT r))
    -> m (OpenADT r)
nbRegsWith normalize p a xs k = go a xs where
    go a [] = k []
    go a (e:es) = do
        e' <- normalize a e
        p a e' $ \r -> nbRegsWith normalize p (a -:- r) es k

knormalizeExp
    :: forall m. (MonadError Text m)
    => HashMap Text Int
    -> RegSet
    -> OpenADT FO
    -> m (OpenADT (KNormal Int))
knormalizeExp rho a = \case
    LNum n -> pure $ LNum n
    LBool b -> pure $ LBool b
    LSym s -> pure $ LSym s
    LEmpty -> pure LEmpty
    EVarLocal x
      | Just r <- HashMap.lookup x rho -> pure $ EVarLocal r
      | otherwise -> throwError $ "Reference to invalid variable " <> x
    EBegin es -> foldl1 ESeq <$> traverse (knormalizeExp rho a) es
    EPrimApply p es ->
        nbRegsWith (knormalizeExp rho) bindAnyReg a es $ \regs ->
            pure $ KNPrimApp p regs Nothing
    EApply f es ->
        knormalizeExp rho a f >>= \f' ->
            bindSmallest a f' $ \fReg ->
                nbRegsWith (knormalizeExp rho) bindSmallest a es $ \eRegs ->
                    pure $ KNApply fReg eRegs
    ELet (unzip -> (names, es)) body ->
        nbRegsWith (knormalizeExp rho) bindAnyReg a es $ \regs ->
            let a' = foldl' (-:-) a regs
                rho' = HashMap.fromList (zip names regs) <> rho
             in knormalizeExp rho' a' body
    EWhile e1 e2 ->
        KNWhile (smallest a) <$> knormalizeExp rho a e1 <*> knormalizeExp rho a e2
    EVarGlobal x ->
        pure $ KNPrimApp (prim "getglobal") ([] @Int) (Just (LSym x))
    EIf e1 e2 e3 -> do
        e1' <- knormalizeExp rho a e1
        bindAnyReg a e1' $ \x ->
            KNIf x <$> knormalizeExp rho a e2 <*> knormalizeExp rho a e3
    KNClosure (KNLambdaF' formals body) captured ->
        KNClosure <$> funcode formals body <*> traverse (knormalizeExp rho a) captured
    KNCaptured n -> pure $ KNCaptured n
    ELetRec (unzip -> (names, closures)) body -> do
        let ts = [smallest a .. smallest a + length names - 1]
            a' = RS (smallest a + length names)
            rho' = HashMap.fromList (zip names ts) <> rho
            closure :: OpenADT FO
                    -> (KNLambdaF Int (OpenADT (KNormal Int)) -> OpenADT (KNormal Int))
                    -> m (OpenADT (KNormal Int))
            closure (ELambda args body) f = do
                body' <- knormalizeExp rho a body
                undefined
         in error "could not get this to work"
    _ -> throwError "NYI"

funcode :: MonadError Text m
        => [Text]
        -> OpenADT FO
        -> m (KNLambdaF Int (OpenADT (KNormal Int)))
funcode args body =
    KNLambdaF' [1 .. length args - 1] <$>
        knormalizeExp (HashMap.fromList (zip args [1..])) (RS (length args)) body

knormalize
    :: forall m. (MonadError Text m)
    => OpenADT FO
    -> m (OpenADT (KNormal Int))
knormalize = knormalizeExp mempty (RS 0)

