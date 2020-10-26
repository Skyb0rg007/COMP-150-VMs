
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Uft.UnambiguousScheme.ToKNormal
    ( unambToKNorm
    ) where

import           Control.Monad.Except
import           Data.Foldable             (traverse_)
import           Data.Foldable             (toList)
import           Data.Functor.Foldable.TH  (makeBaseFunctor)
import           Data.Maybe                (isJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import qualified Uft.Asm.Ast as Asm
import qualified Uft.UnambiguousScheme.Ast as U
import qualified Uft.KNormal.Ast as KN
import           Uft.Scheme.Prims

unambToKNorm
    :: MonadError Text m
    => U.Prog
    -> m [KN.Exp Text]
unambToKNorm = traverse cvtStmt

cvtStmt
    :: MonadError Text m
    => U.Stmt
    -> m (KN.Exp Text)
cvtStmt = \case
    U.Exp (U.ExpLet U.Let [(t, U.ExpLambda args e)] (U.ExpSetLocal f (U.ExpLocalVar t')))
      | t == t' -> do
          e' <- cvtExp e
          pure $ KN.ExpLet t (KN.ExpFunCode args e') (KN.ExpLitCmd PrimSetGlobal [t] (KN.LitSym f))
    U.Exp e -> cvtExp e
    U.Define f args body -> do
        body' <- cvtExp body
        let t = "@temp@"
        pure $ KN.ExpLet t (KN.ExpFunCode args body') (KN.ExpLitCmd PrimSetGlobal [t] (KN.LitSym f))
    U.Val x e -> do
        e' <- cvtExp e
        let t = "@temp@"
        pure $ KN.ExpLet t e' (KN.ExpLitCmd PrimSetGlobal [t] (KN.LitSym x))

cvtExp
    :: MonadError Text m
    => U.Exp
    -> m (KN.Exp Text)
cvtExp = \case
    U.ExpLit lit -> pure $ KN.ExpLit $ cvtLit lit
    U.ExpLocalVar x -> pure $ KN.ExpVar x
    U.ExpGlobalVar x -> pure $ KN.ExpLitCmd PrimGetGlobal [] (KN.LitSym x)
    U.ExpSetLocal x e -> KN.ExpSet x <$> cvtExp e
    U.ExpSetGlobal x e -> pure $ KN.ExpLitCmd PrimSetGlobal [] (KN.LitSym x)
    U.ExpIf e1 e2 e3 -> KN.ExpIf <$> cvtExp e1 <*> cvtExp e2 <*> cvtExp e3
    U.ExpWhile (U.ExpLet U.Let [(x, e1)] (U.ExpLocalVar y)) e2
      | x == y -> KN.ExpWhile x <$> cvtExp e1 <*> cvtExp e2
    U.ExpBegin [] -> pure $ KN.ExpLit KN.LitNil
    U.ExpBegin es -> foldl1 KN.ExpSeq <$> traverse cvtExp es
    U.ExpFunApply (U.ExpLocalVar f) args
      | all isVar args -> pure $ KN.ExpFunCall f (fmap getVar args)
    U.ExpPrimApply f args 
      | all isVar args -> pure $ KN.ExpCmd f (fmap getVar args)
      | all isVar (Vector.init args) 
      , U.ExpLit l <- Vector.last args -> pure $
          KN.ExpLitCmd f (fmap getVar (Vector.init args)) (cvtLit l)
    U.ExpLet U.Let [(x, e1)] e2 -> KN.ExpLet x <$> cvtExp e1 <*> cvtExp e2
    e -> throwError $ "Unable to convert to KNormal form: " <> Text.pack (show e)

isVar (U.ExpLocalVar _) = True
isVar _ = False

getVar (U.ExpLocalVar x) = x
getVar _ = error "Shouldn't happen"

cvtLit :: U.Literal -> KN.Literal
cvtLit = \case
    U.LitSym x  -> KN.LitSym x
    U.LitNum n  -> KN.LitNum n
    U.LitBool b -> KN.LitBool b
    U.LitEmpty  -> KN.LitEmpty

