{-# LANGUAGE TemplateHaskell #-}

module Uft.KNormal.FromUnamb
    ( module Uft.KNormal.FromUnamb
    ) where

import           Control.Monad.Except
import           Data.Deriving
import           Data.Foldable          (foldl')
import           Data.Kind
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Primitives
import           Uft.Scheme.Ast
import           Uft.Scheme.ConvertPrim
import           Uft.Scheme.Disambiguate
import           Uft.Scheme.ListExpand
import           Uft.Asm.Ast (Literal)
import           Uft.Util

type KNormal name =
    '[ ExpPrimApplyF name
     , ExpPrimApplyLitF name
     , ExpFunApplyF name
     , ExpLet1F name
     , ExpIfF
     , ExpWhileF
     , ExpGetLocalF name
     , ExpSetLocalF name
     , ExpBeginF
     , LitNumF
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

data ExpFunApplyF name (a :: Type) = ExpFunApplyF' !name !(Vector name)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data ExpLet1F name a = ExpLet1F' !name !a !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

derive [deriveShow1, deriveEq1, deriveOrd1, deriveOpenADT]
    [''ExpPrimApplyLitF,''ExpPrimApplyF, ''ExpLet1F, ''ExpFunApplyF]

fromUnamb
    :: forall r m old new.
        ( old ~ '[ExpLetF, ExpLetRecF, ExpSetGlobalF, ExpApplyF]
        , new ~ '[ExpPrimApplyF Text, ExpPrimApplyLitF Text, ExpLet1F Text, ExpFunApplyF Text]
        , old :<: r
        , '[ExpPrimF, ExpGetLocalF Text, LitNumF, LitStrF, LitSymF, LitEmptyF, LitCharF]
            :<: new ++ (r \\ old)
        , Applies '[Functor, Foldable, Traversable] r
        , MonadError Text m
        )
   => OpenADT r
   -> m (OpenADT (new ++ (r \\ old)))
fromUnamb = cataM alg where
    fromLit :: OpenADT (new ++ (r \\ old)) -> m Literal
    fromLit (LitNum n) = pure $ LitNum n
    fromLit (LitStr s) = pure $ LitStr s
    fromLit (LitSym s) = pure $ LitSym s
    fromLit LitEmpty = pure LitEmpty
    fromLit _ = throwError "Invalid literal"
    isVar :: OpenADT (new ++ (r \\ old)) -> Maybe Text
    isVar (ExpGetLocal x) = Just x
    isVar _ = Nothing
    alg :: Sum r (OpenADT (new ++ (r \\ old))) -> m (OpenADT (new ++ (r \\ old)))
    alg x =
        case decompose4 x of
          L1 (ExpLetF' (Vector.toList -> [(x, e)]) body) -> pure $ ExpLet1 x e body
          L1 (ExpLetF' {})         -> throwError "unable to translate"
          R1 (L1 (ExpLetRecF' {})) -> throwError "unable to translate"
          R1 (R1 (L1 (ExpSetGlobalF' x e))) ->
              case e of
                ExpGetLocal (y :: Text) -> pure $ ExpPrimApply (prim "getglobal") (Vector.singleton y)
                _ -> throwError "unable to translate"
          R1 (R1 (R1 (L1 e))) ->
              case e of
                ExpApplyF' (ExpPrim p) args
                  | _prim_arity p == length args
                  , not (_prim_lit p)
                  , Just args' <- traverse isVar args -> pure $ ExpPrimApply p args'
                ExpApplyF' (ExpPrim p) args
                  | _prim_arity p == length args
                  , _prim_lit p
                  , Just args' <- traverse isVar (Vector.init args) ->
                      ExpPrimApplyLit p args' <$> fromLit (Vector.last args)
                  | otherwise -> throwError $ "Primitive " <> tshow (_prim_name p) <> " is applied to the wrong number of args"
                ExpApplyF' (ExpGetLocal x) args 
                  | Just args' <- traverse isVar args -> pure $ ExpFunApply x args'
          R1 (R1 (R1 (R1 x'))) -> pure $ Fix $ weaken4 x'

