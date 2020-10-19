
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
import           Data.Hashable                         (Hashable)
import           Data.HashSet                          (HashSet)
import qualified Data.HashSet                          as HashSet
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Vector                           (Vector)
import qualified Data.Vector                           as Vector
import qualified Uft.Scheme.Ast                        as In
import           Uft.Scheme.Prims
import qualified Uft.UnambiguousScheme.Ast             as Out
import           Data.Foldable (foldlM)

-- Construct a HashSet from a foldable structure
hashSet
    :: (Hashable a, Eq a, Foldable f)
    => f a
    -> HashSet a
hashSet = foldr HashSet.insert HashSet.empty

-- Convert a pretty value to strict text
prettyText
    :: Pretty a
    => a
    -> Text
prettyText = renderStrict . layoutPretty defaultLayoutOptions . pretty

-- Determine if a variable is a primitive, returning the enum + arity if it is
isPrim
    :: Text
    -> Maybe (Prim, Int)
isPrim x =
    case primParse x of
      Nothing -> Nothing
      Just p -> Just (p, primArity p)

-- Used to ensure all primitives are fully saturated
etaExpand
    :: Prim
    -> Out.Exp
etaExpand p = Out.ExpLambda args $ Out.ExpPrimApply p (Out.ExpLocalVar <$> args)
    where
        args :: Vector Text
        args = Vector.fromList $
            take (primArity p) $
                map Text.singleton ['a' .. 'z']

disambiguate
    :: MonadError Text m
    => In.Prog
    -> m Out.Prog
disambiguate = traverse disambiguateStmt

-- | Convert the ambiguous Scheme syntax into unambiguous scheme
disambiguateStmt
    :: forall m. MonadError Text m
    => In.Stmt
    -> m Out.Stmt
disambiguateStmt = \case
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
    In.ExpLet In.Let binds body ->
        let go' = disambiguateExp (hashSet (fmap fst binds) <> locals)
         in Out.ExpLet Out.Let
            <$> (traverse . traverse) go binds
            <*> fmap Out.ExpBegin (traverse go' body)
    In.ExpLet In.LetStar binds body -> do
        let f :: (HashSet Text, Out.Exp -> Out.Exp) -> (Text, In.Exp) -> m (HashSet Text, Out.Exp -> Out.Exp)
            f (locals, cont) (x, e) = do
                e' <- disambiguateExp locals e
                pure (HashSet.insert x locals, cont . Out.ExpLet Out.Let [(x, e')])
        (locals', cont) <- foldlM f (locals, id) binds
        body' <- Out.ExpBegin <$> traverse (disambiguateExp locals') body
        pure $ cont body'
    In.ExpLet In.LetRec binds body ->
        let locals' = hashSet (fmap fst binds) <> locals
            go' = disambiguateExp locals'
         in Out.ExpLet Out.LetRec <$> (traverse . traverse) go' binds <*> fmap Out.ExpBegin (traverse go' body)
    In.ExpApply (In.ExpVar f) args
      | Just (p, n) <- isPrim f ->
          if Vector.length args == n
             then Out.ExpPrimApply p <$> traverse go args
             else Out.ExpFunApply (etaExpand p) <$> traverse go args
    In.ExpApply f args -> Out.ExpFunApply <$> go f <*> traverse go args
    In.ExpLambda args e -> Out.ExpLambda args <$> go e
    where
        go = disambiguateExp locals
    

disambiguateLit :: In.Literal -> Out.Exp
disambiguateLit = \case
    In.LitBool b  -> Out.ExpLit $ Out.LitBool b
    In.LitSym sym -> Out.ExpLit $ Out.LitSym sym
    In.LitNum n   -> Out.ExpLit $ Out.LitNum n
    In.LitEmpty   -> Out.ExpLit Out.LitEmpty
    In.LitPair a b -> Out.ExpPrimApply PrimCons
        [ disambiguateLit a
        , disambiguateLit b
        ]

