{-# LANGUAGE TemplateHaskell #-}
-- Closure conversion
module Language.Scheme.L3
    ( module Language.Scheme.L3
    , L1Constant (..)
    ) where

import           Control.Lens               (preview)
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Data.Bifunctor             (first)
import           Data.Functor.Foldable      hiding (embed, project)
import qualified Data.Functor.Foldable      as Foldable
import           Data.Functor.Foldable.TH
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HashSet
import           Data.Text                  (Text)
import           Data.Unique
import           Data.Maybe
import           Language.Scheme.L2         (L2, L1Constant (..))
import qualified Language.Scheme.L2         as L2
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Class
import           System.IO.Unsafe           (unsafePerformIO)
import           Uft.Primitives
import           Uft.Util

type L3Constant = L1Constant

data L3
    = EConst L3Constant
    | ELet [(Unique, L3)] L3
    | EBegin [L3]
    | ELocalSet Unique L3
    | EGlobalSet Text L3
    | EIf L3 L3 L3
    | EWhile L3 L3
    | ELocalVar Unique
    | EGlobalVar Text
    | EClosedVar Int
    | EApply L3 [L3]
    | EPrimApply Prim [L3]
    | EClosure {- Lambda -} ([Unique], L3) {- Closed -} [L3]

makeBaseFunctor ''L3

instance Embed L3 where
    embed = \case
        EConst k             -> embed k
        ELet bs e            -> SList ["let", SList (embedBinds bs), embed e]
        EBegin es            -> SList ("begin" : map embed es)
        ELocalSet x e        -> SList ["set!", unique x, embed e]
        EGlobalSet x e       -> SList ["set-global!", SSymbol x, embed e]
        EIf e1 e2 e3         -> SList ["if", embed e1, embed e2, embed e3]
        EWhile e1 e2         -> SList ["while", embed e1, embed e2]
        ELocalVar x          -> unique x
        EGlobalVar x         -> SList ["global", SSymbol x]
        EApply f args        -> SList (embed f : map embed args)
        EPrimApply p args    -> SList (SSymbol (_prim_name p) : map embed args)
        EClosure (args, e) c -> SList ("closure" : SList ["lambda", SList (map unique args), embed e] : map embed c)
        EClosedVar n         -> SList ["getclslot", SNum (fromIntegral n)]
        where
            embedBinds = map $ \(x, e) -> SList [unique x, embed e]
            unique x = SSymbol $ "x" <> tshow (hashUnique x)

instance Project L3 where
    project = fmap (fmap (closureConvert HashMap.empty)) . project

closureConvert :: HashMap Unique Int -> L2 -> L3
closureConvert env = go where
    go = \case
        L2.EConst k -> EConst k
        L2.ELet binds body -> ELet (map (fmap go) binds) (go body)
        e@(L2.ELambda args body) ->
            let fv = HashSet.toList (freeVars e)
                fvMap = HashMap.fromList (zip fv [0..])
             in EClosure (args, closureConvert fvMap body) (map ELocalVar fv)
        L2.EBegin es -> EBegin (map go es)
        L2.ELocalSet u e -> ELocalSet u (go e)
        L2.EGlobalSet x e -> EGlobalSet x (go e)
        L2.EIf a b c -> EIf (go a) (go b) (go c)
        L2.EWhile a b -> EWhile (go a) (go b)
        L2.ELocalVar x 
          | Just n <- HashMap.lookup x env -> EClosedVar n
          | otherwise -> ELocalVar x
        L2.EGlobalVar x -> EGlobalVar x
        L2.EApply f args -> EApply (go f) (map go args)
        L2.EPrimApply p args -> EPrimApply p (map go args)
    freeVars :: L2 -> HashSet Unique
    freeVars = \case
        L2.EConst k -> mempty
        L2.ELet binds body -> foldMap (freeVars . snd) binds <> HashSet.difference (freeVars body) (HashSet.fromList (map fst binds))
        L2.ELambda args body -> HashSet.difference (freeVars body) (HashSet.fromList args)
        L2.EBegin es -> foldMap freeVars es
        L2.ELocalSet u e -> HashSet.insert u (freeVars e)
        L2.EGlobalSet x e -> freeVars e
        L2.EIf a b c -> freeVars a <> freeVars b <> freeVars c
        L2.EWhile a b -> freeVars a <> freeVars b
        L2.ELocalVar x -> HashSet.singleton x
        L2.EGlobalVar x -> mempty
        L2.EApply f args -> freeVars f <> foldMap freeVars args
        L2.EPrimApply _ args -> foldMap freeVars args

