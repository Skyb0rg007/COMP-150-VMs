
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Uft.Scheme.Disambiguate
    ( disambiguate
    ) where

import           Control.Monad.Except
import           Data.Foldable                         (traverse_)
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH              (makeBaseFunctor)
import           Data.Hashable                         (Hashable)
import           Data.HashSet                          (HashSet)
import qualified Data.HashSet                          as HashSet
import           Data.Maybe                            (isJust)
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Vector                           (Vector)
import qualified Data.Vector                           as Vector

import qualified Uft.Scheme.Ast         as In
import qualified Uft.Scheme.Unambiguous as Out

hashSet
    :: (Hashable a, Eq a, Foldable f)
    => f a
    -> HashSet a
hashSet = foldr HashSet.insert HashSet.empty

prettyText
    :: Pretty a
    => a
    -> Text
prettyText = renderStrict . layoutPretty defaultLayoutOptions . pretty

isPrim
    :: Text
    -> Maybe (Out.Prim, Int)
isPrim "cons" = Just (Out.PrimCons, 2)
isPrim _ = Nothing

disambiguate
    :: forall m. MonadError Text m
    => In.Stmt
    -> m Out.Stmt
disambiguate = \case
    In.Val x e -> Out.Val x <$> disambiguateExp HashSet.empty e
    In.Define x args e -> Out.Define x args <$> disambiguateExp (hashSet args) e
    In.Exp e -> Out.Exp <$> disambiguateExp HashSet.empty e
    In.CheckExpect e1 e2 ->
        Out.CheckExpect
        <$> ((prettyText e1,) <$> disambiguateExp HashSet.empty e1)
        <*> ((prettyText e2,) <$> disambiguateExp HashSet.empty e2)
    In.CheckAssert e ->
        Out.CheckAssert <$> ((prettyText e,) <$> disambiguateExp HashSet.empty e)


-- | Disambiguate, passing along the current locals
-- NOTE: I don't eta-expand primitives, since primitives
-- must always be saturated in my language, where primitives have different
-- variable names from the normally-used operators.
disambiguateExp
    :: forall m. MonadError Text m
    => HashSet Text
    -> In.Exp
    -> m Out.Exp
disambiguateExp locals = \case
    In.ExpLit lit -> pure $ disambiguateLit lit
    In.ExpVar x
      | x `HashSet.member` locals -> pure $ Out.ExpLocalVar x
      | otherwise -> pure $ Out.ExpGlobalVar x
    In.ExpSet x e
      | x `HashSet.member` locals -> Out.ExpSetLocal x <$> go e
      | otherwise -> Out.ExpSetGlobal x <$> go e
    In.ExpIf e1 e2 e3 -> Out.ExpIf <$> go e1 <*> go e2 <*> go e3
    In.ExpWhile e1 e2 -> Out.ExpWhile <$> go e1 <*> go e2
    In.ExpBegin es -> Out.ExpBegin <$> traverse go es
    In.ExpLet In.Let binds e ->
        Out.ExpLet Out.Let
        <$> (traverse . traverse) go binds
        <*> disambiguateExp (hashSet (fmap fst binds) <> locals) e
    In.ExpApply (In.ExpVar f) args
      | Just (p, n) <- isPrim f ->
          if Vector.length args == n 
             then Out.ExpPrimApply p <$> traverse go args
             else throwError $ "Wrong number of args to prim '" <> f <> "'"
    In.ExpApply f args -> Out.ExpFunApply <$> go f <*> traverse go args
    In.ExpLet In.LetRec binds e ->
        let go' = disambiguateExp locals'
            locals' = hashSet (fmap fst binds) <> locals
         in Out.ExpLet Out.LetRec <$> (traverse . traverse) go' binds <*> go' e
    In.ExpLambda args e -> Out.ExpLambda args <$> go e
    where
        go = disambiguateExp locals
    

disambiguateLit :: In.Literal -> Out.Exp
disambiguateLit = \case
    In.LitBool b  -> Out.ExpLit $ Out.LitBool b
    In.LitSym sym -> Out.ExpLit $ Out.LitSym sym
    In.LitNum n   -> Out.ExpLit $ Out.LitNum n
    In.LitEmpty   -> Out.ExpLit Out.LitEmpty
    In.LitPair a b -> Out.ExpPrimApply Out.PrimCons
        [ disambiguateLit a
        , disambiguateLit b
        ]

