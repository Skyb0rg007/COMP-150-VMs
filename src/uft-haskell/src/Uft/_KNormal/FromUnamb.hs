{-# LANGUAGE TemplateHaskell #-}

module Uft.KNormal.FromUnamb
    ( module Uft.KNormal.FromUnamb
    ) where

import           Control.Monad.Except
import           Data.Deriving
import           Data.Foldable           (foldl')
import           Data.Kind
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Text               (Text)
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Primitives
import           Uft.Scheme.Ast
import           Uft.Scheme.ConvertPrim
import           Uft.Scheme.Disambiguate
import           Uft.Scheme.ListExpand
import           Uft.Util

type Literal = OpenADT
    '[ LitNumF
     , LitStrF
     , LitSymF
     , LitCharF
     , LitBoolF
     , LitEmptyF
     , LitNilF
     ]

data LitNilF (a :: Type) = LitNilF'
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

derive [deriveShow1, deriveEq1, deriveOrd1, deriveOpenADT]
    [''LitNilF]

type KNormal name =
    '[ ExpPrimApplyF name
     , ExpPrimApplyLitF name
     , ExpFunApplyF name
     , ExpKLetF name
     , ExpKIfF name
     , ExpKWhileF name
     , ExpGetLocalF name
     , ExpSetLocalF name
     , ExpBeginF
     , LitNumF
     , LitNilF
     , LitStrF
     , LitSymF
     , LitCharF
     , LitBoolF
     , LitEmptyF
     ]

data ExpPrimApplyLitF name (a :: Type) = ExpPrimApplyLitF' !Prim !(Vector name) !Literal
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data ExpPrimApplyF name (a :: Type) = ExpPrimApplyF' !Prim !(Vector name)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data ExpPrimApplyFOF a = ExpPrimApplyFOF' !Prim !(Vector a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data ExpFunApplyF name (a :: Type) = ExpFunApplyF' !name !(Vector name)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data ExpKLetF name a = ExpKLetF' !name !a !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data ExpKIfF name a = ExpKIfF' !name !a !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data ExpKWhileF name a = ExpKWhileF' !name !a !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

derive [deriveShow1, deriveEq1, deriveOrd1, deriveOpenADT]
    [''ExpPrimApplyLitF,''ExpPrimApplyF, ''ExpKLetF, ''ExpFunApplyF, ''ExpKIfF, ''ExpKWhileF, ''ExpPrimApplyFOF]

-- * Helpers

newtype RegSet = RS Int

smallest :: RegSet -> Int
smallest (RS n) = n

(-:-) :: RegSet -> Int -> RegSet
(-:-) (RS n) r = RS (max n (r + 1))

bindAnyReg
    :: forall r m. ('[ExpKLetF Int, ExpGetLocalF Int] :<: r, Monad m)
    => RegSet
    -> OpenADT r
    -> (Int -> m (OpenADT r))
    -> m (OpenADT r)
bindAnyReg a (ExpGetLocal n) k = k n
bindAnyReg a e k = bindSmallest a e k

bindSmallest
    :: forall r m. (ExpKLetF Int :< r, Monad m)
    => RegSet
    -> OpenADT r
    -> (Int -> m (OpenADT r))
    -> m (OpenADT r)
bindSmallest a e k =
    let r = smallest a
     in ExpKLet r e <$> k r

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

type FO =
    '[ExpPrimApplyFOF, ExpApplyF, ExpIfF, ExpWhileF, ExpLetF, ExpGetLocalF Text, ExpBeginF,
      LitNumF, LitStrF, LitSymF, LitEmptyF]

knormalizeExp
    :: forall r m.
        ( FO :<: r
        , Apply Functor r
        , MonadError Text m
        )
    => Map Text Int
    -> RegSet
    -> OpenADT r
    -> m (OpenADT (KNormal Int))
knormalizeExp rho a = \case
    LitNum n -> pure $ LitNum n
    LitStr s -> pure $ LitStr s
    LitSym s -> pure $ LitSym s
    LitEmpty -> pure LitEmpty
    ExpGetLocal x
      | Just r <- Map.lookup x rho -> pure $ ExpGetLocal r
    ExpBegin (Vector.toList -> es) ->
        ExpBegin . Vector.fromList <$> traverse (knormalizeExp rho a) es
    ExpPrimApplyFO p (Vector.toList -> es) ->
        nbRegsWith (knormalizeExp rho) bindAnyReg a es $ \regs ->
            pure $ ExpPrimApply p (Vector.fromList regs)
    ExpApply f (Vector.toList -> args) ->
        knormalizeExp rho a f >>= \e' ->
            bindSmallest a e' $ \r ->
                nbRegsWith (knormalizeExp rho) bindSmallest a args $ \regs ->
                    pure $ ExpFunApply r (Vector.fromList regs)
    ExpLet (Vector.toList -> binds) body ->
        nbRegsWith (knormalizeExp rho) bindAnyReg a (map snd binds) $ \regs ->
            let a' = foldl' (-:-) a regs
                rho' = Map.fromList (zip (map fst binds) regs) <> rho
             in knormalizeExp rho' a' body
    _ -> error "NYI"

-- toFO
    -- :: forall r m old new.
        -- ( old ~ '[ExpLetF, ExpLetRecF, ExpSetGlobalF, ExpApplyF]
        -- , new ~ '[ExpPrimApplyF Text, ExpPrimApplyLitF Text, ExpKLetF Text, ExpFunApplyF Text]
        -- , old :<: r
        -- , '[ExpPrimF, ExpGetLocalF Text, LitNumF, LitStrF, LitSymF, LitEmptyF, LitCharF]
            -- :<: new ++ (r \\ old)
        -- , Applies '[Functor, Foldable, Traversable] r
        -- , MonadError Text m
        -- )
   -- => OpenADT r
   -- -> m (OpenADT (new ++ (r \\ old)))
-- toFO = cataM alg where
    -- fromLit :: OpenADT (new ++ (r \\ old)) -> m Literal
    -- fromLit (LitNum n) = pure $ LitNum n
    -- fromLit (LitStr s) = pure $ LitStr s
    -- fromLit (LitSym s) = pure $ LitSym s
    -- fromLit LitEmpty = pure LitEmpty
    -- fromLit _ = throwError "Invalid literal"
    -- isVar :: OpenADT (new ++ (r \\ old)) -> Maybe Text
    -- isVar (ExpGetLocal x) = Just x
    -- isVar _ = Nothing
    -- alg :: Sum r (OpenADT (new ++ (r \\ old))) -> m (OpenADT (new ++ (r \\ old)))
    -- alg x =
        -- case decompose4 x of
          -- L1 (ExpLetF' (Vector.toList -> [(x, e)]) body) -> pure $ ExpKLet x e body
          -- L1 (ExpLetF' {})         -> throwError "unable to translate"
          -- R1 (L1 (ExpLetRecF' {})) -> throwError "unable to translate"
          -- R1 (R1 (L1 (ExpSetGlobalF' x e))) ->
              -- case e of
                -- ExpGetLocal (y :: Text) -> pure $ ExpPrimApply (prim "getglobal") (Vector.singleton y)
                -- _ -> throwError "unable to translate"
          -- R1 (R1 (R1 (L1 e))) ->
              -- case e of
                -- ExpApplyF' (ExpPrim p) args
                  -- | _prim_arity p == length args
                  -- , not (_prim_lit p)
                  -- , Just args' <- traverse isVar args -> pure $ ExpPrimApply p args'
                -- ExpApplyF' (ExpPrim p) args
                  -- | _prim_arity p == length args
                  -- , _prim_lit p
                  -- , Just args' <- traverse isVar (Vector.init args) ->
                      -- ExpPrimApplyLit p args' <$> fromLit (Vector.last args)
                  -- | otherwise -> throwError $ "Primitive " <> tshow (_prim_name p) <> " is applied to the wrong number of args"
                -- ExpApplyF' (ExpGetLocal x) args 
                  -- | Just args' <- traverse isVar args -> pure $ ExpFunApply x args'
          -- R1 (R1 (R1 (R1 x'))) -> pure $ Fix $ weaken4 x'

