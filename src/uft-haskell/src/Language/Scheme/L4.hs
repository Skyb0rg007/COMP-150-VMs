{-# LANGUAGE TemplateHaskell #-}
-- Register allocation and k-normalization
module Language.Scheme.L4
    ( module Language.Scheme.L4
    , L1Constant (..)
    ) where

import           Control.Lens               (preview)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bifunctor             (first)
import           Data.Functor.Foldable      hiding (embed, project)
import qualified Data.Functor.Foldable      as Foldable
import           Data.Functor.Foldable.TH
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HashSet
import           Data.Maybe
import           Data.Text                  (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Unique
import           Debug.Trace
import           Language.Scheme.L3         (L1Constant (..), L3)
import qualified Language.Scheme.L3         as L3
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Class
import           System.IO.Unsafe           (unsafePerformIO)
import           Uft.Primitives
import           Uft.Util

type L4Constant = L1Constant

data L4
    = EConst L4Constant
    | EName Int
    | EPrim Prim [Int]
    | EPrimLit Prim [Int] L4Constant
    | EFuncall Int [Int]
    | EIf Int L4 L4
    | ELet Int L4 L4
    | ESeq L4 L4
    | EWhile Int L4 L4
    | ECaptured Int
    | EClosure ([Int], L4) [Int]
    | ELocalSet Int L4

instance Project L4 where
    project = traverse (knormalize HashMap.empty (RS 0)) <=< project

instance Embed L4 where
    embed = \case
        EConst k                -> embed k
        EName n                 -> name n
        EPrim p args            -> SList (SSymbol (_prim_name p) : map name args)
        EPrimLit p args lit     -> SList (SSymbol (_prim_name p) : map name args ++ [embed lit])
        EFuncall f args         -> SList (name f : map name args)
        EIf a b c               -> SList ["if", name a, embed b, embed c]
        ELet x a b              -> SList ["let", name x, embed a, embed b]
        ESeq a b                -> SList ["seq", embed a, embed b]
        EWhile x a b            -> SList ["while", SList ["let", SList [SList [name x, embed a]]], embed b]
        ECaptured n             -> SList ["getclslot", SNum (fromIntegral n)]
        EClosure (args, body) c -> SList ("closure" : SList ["lambda", SList (map name args), embed body] : map name c)
        ELocalSet x e           -> SList ["set!", name x, embed e]
        where
            name n = SSymbol $ "r" <> tshow n

-- * Helpers

newtype RegSet = RS Int

smallest :: RegSet -> Int
smallest (RS n) = n

(-:-) :: RegSet -> Int -> RegSet
(-:-) (RS n) r = RS (max n r + 1)

bindAnyReg
    :: forall r m. Monad m
    => RegSet
    -> L4
    -> (Int -> m L4)
    -> m L4
bindAnyReg a (EName n) k = k n
bindAnyReg a e k = bindSmallest a e k

bindSmallest
    :: forall r m. Monad m
    => RegSet
    -> L4
    -> (Int -> m L4)
    -> m L4
bindSmallest a e k =
    let r = smallest a
     in ELet r e <$> k r

nbRegsWith
    :: Monad m
    => (RegSet -> a -> m L4)
    -> (RegSet -> L4 -> (Int -> m L4) -> m L4)
    -> RegSet
    -> [a]
    -> ([Int] -> m L4)
    -> m L4
nbRegsWith normalize p a xs k = go a xs where
    go a [] = k []
    go a (e:es) = do
        e' <- normalize a e
        p a e' $ \r -> nbRegsWith normalize p (a -:- r) es (k . (r:))

instance Show Unique where
    show u = "#" ++ show (hashUnique u)

knormalize
    :: Monad m
    => HashMap Unique Int
    -> RegSet
    -> L3
    -> m L4
knormalize rho a = \case
    L3.EConst k -> pure $ EConst k
    L3.EPrimApply p args ->
        nbRegs bindAnyReg a args $ \args' ->
            pure $ EPrim p args'
    L3.EApply f args -> do
        f' <- knormalize rho a f
        bindSmallest a f' $ \r ->
            nbRegs bindSmallest (a -:- r) args $ \args' ->
                pure $ EFuncall r args'
    L3.ELocalSet x e ->
        let r = rho HashMap.! x
         in ELocalSet r <$> knormalize rho a e
    L3.EGlobalVar x ->
        pure $ EPrimLit (prim "getglobal") [] (KSymbol x)
    L3.EGlobalSet x e -> do
        e' <- knormalize rho a e
        bindAnyReg a e' $ \r ->
            pure $ EPrimLit (prim "setglobal") [r] (KSymbol x)
    L3.EBegin [] -> pure $ EPrim (prim "void") []
    L3.EBegin [e] -> knormalize rho a e
    L3.EBegin (e:es) -> ESeq <$> knormalize rho a e <*> knormalize rho a (L3.EBegin es)
    L3.EIf e1 e2 e3 -> do
        e1' <- knormalize rho a e1
        e2' <- knormalize rho a e2
        e3' <- knormalize rho a e3
        bindAnyReg a e1' $ \r ->
            pure $ EIf r e2' e3'
    L3.EWhile e1 e2 -> do
        let r = smallest a
        e1' <- knormalize rho a e1
        e2' <- knormalize rho a e2
        pure $ EWhile r e1' e2'
    L3.ELet (unzip -> (names, es)) body ->
        nbRegs bindAnyReg a es $ \rs ->
            let rho' = HashMap.fromList (zip names rs) <> rho
             in knormalize rho' (removeRegisters a rs) body
    L3.EClosure (args, body) closed ->
        nbRegs bindAnyReg a closed $ \rs ->
            EClosure <$> funcode (args, body) (rho, RS 1) <*> pure rs
    L3.EClosedVar n -> pure $ ECaptured n
    L3.ELocalVar x -> 
        case HashMap.lookup x rho of
          Nothing -> error $ "Missing " ++ show x ++ " in " ++ show rho
          Just n -> pure (EName n)
    where
        removeRegisters rs [] = rs
        removeRegisters rs (x:xs) = removeRegisters (rs -:- x) xs
        nbRegs = nbRegsWith (knormalize rho)
        funcode (args, body) (rho, rs) =
            let f (bindings, a) x = (HashMap.insert x (smallest a) bindings, a -:- smallest a)
                (rho', rs') = foldl f (rho, rs) args
                consec = (rho' HashMap.!) <$> args
             in (consec,) <$> knormalize rho' rs' body
