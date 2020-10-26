
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Uft.KNormal.ToScheme
    ( knormToScheme
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
import qualified Uft.Scheme.Ast as Scheme
import qualified Uft.KNormal.Ast as KN
import           Uft.Scheme.Prims

knormToScheme :: KN.Exp Text -> Scheme.Prog
knormToScheme = pure . Scheme.Exp . cvtExp

cvtExp :: KN.Exp Text -> Scheme.Exp
cvtExp = \case
    KN.ExpLit lit -> Scheme.ExpLit $ cvtLit lit
    KN.ExpVar x   -> Scheme.ExpVar x
    KN.ExpIf e1 e2 e3 -> Scheme.ExpIf (cvtExp e1) (cvtExp e2) (cvtExp e3)
    KN.ExpLet x e1 e2 ->
        Scheme.ExpLet Scheme.Let
            (Vector.singleton (x, cvtExp e1))
            (Vector.singleton (cvtExp e2))
    KN.ExpSeq e1 e2 ->
        Scheme.ExpBegin $ Vector.fromList (map cvtExp [e1, e2])
    KN.ExpSet x e ->
        Scheme.ExpSet x (cvtExp e)
    KN.ExpWhile x e1 e2 -> do
        Scheme.ExpWhile
            (Scheme.ExpLet Scheme.Let
                (Vector.singleton (x, cvtExp e1))
                (Vector.singleton $ Scheme.ExpVar x))
            (cvtExp e2)
    KN.ExpFunCode args body ->
        Scheme.ExpLambda args (cvtExp body)
    KN.ExpFunCall f args ->
        Scheme.ExpApply (Scheme.ExpVar f) (fmap Scheme.ExpVar args)
    KN.ExpCmd prim args ->
        Scheme.ExpApply (Scheme.ExpVar (primName prim)) (fmap Scheme.ExpVar args)
    KN.ExpLitCmd prim args lit ->
        Scheme.ExpApply
            (Scheme.ExpVar (primName prim))
            (Vector.snoc (fmap Scheme.ExpVar args) (Scheme.ExpLit $ cvtLit lit))

cvtLit :: KN.Literal -> Scheme.Literal
cvtLit = \case
    KN.LitNum n  -> Scheme.LitNum n
    KN.LitBool b -> Scheme.LitBool b
    KN.LitSym s  -> Scheme.LitSym s
    KN.LitEmpty  -> Scheme.LitEmpty
