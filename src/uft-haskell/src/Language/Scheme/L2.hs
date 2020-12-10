{-# OPTIONS_GHC -Wall #-}
-- Local variable renaming + mutable variable elim
module Language.Scheme.L2
    ( L2 (..)
    , L2Constant
    , L1Constant (..)
    ) where

-- import           Control.Lens               (preview)
import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
-- import           Data.Bifunctor             (first)
-- import           Data.Functor.Foldable      hiding (embed, project)
-- import           Data.Functor.Foldable.TH
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HashSet
import           Data.Text                  (Text)
import           Data.Unique
import           Data.Maybe
import           Language.Scheme.L1         (L1, L1Constant (..))
import qualified Language.Scheme.L1         as L1
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Class
import           System.IO.Unsafe           (unsafePerformIO)
import           Language.Scheme.Primitives
import           Language.Scheme.Util

type L2Constant = L1Constant

data L2
    = EConst L2Constant
    | ELet [(Unique, L2)] L2
    | ELambda [Unique] L2
    | EBegin [L2]
    | ELocalSet Unique L2
    | EGlobalSet Text L2
    | EIf L2 L2 L2
    | EWhile L2 L2
    | ELocalVar Unique
    | EGlobalVar Text
    | EApply L2 [L2]
    | EPrimApply Prim [L2]

instance Embed L2 where
    embed = \case
        EConst k          -> embed k
        ELet bs e         -> SList ["let", SList (embedBinds bs), embed e]
        ELambda args e    -> SList ["lambda", SList (map unique args), embed e]
        EBegin es         -> SList ("begin" : map embed es)
        ELocalSet x e     -> SList ["set!", unique x, embed e]
        EGlobalSet x e    -> SList ["set-global!", SSymbol x, embed e]
        EIf e1 e2 e3      -> SList ["if", embed e1, embed e2, embed e3]
        EWhile e1 e2      -> SList ["while", embed e1, embed e2]
        ELocalVar x       -> unique x
        EGlobalVar x      -> SList ["global", SSymbol x]
        EApply f args     -> SList (embed f : map embed args)
        EPrimApply p args -> SList (SSymbol (_prim_name p) : map embed args)
        where
            embedBinds = map $ \(x, e) -> SList [unique x, embed e]
            unique x = SSymbol $ "x" <> tshow (hashUnique x)

instance Project L2 where
    project sexps = do
        l1 <- project sexps
        let l2 = map toL2 l1
        let mut = foldMap detectMutable l2
        pure $ wrapMutable mut <$> l2

toL2 :: L1 -> L2
toL2 = \case
    L1.EConst k          -> EConst k
    L1.ELet bs e         -> ELet (map (fmap toL2) bs) (toL2 e)
    L1.ELambda args e    -> ELambda args (toL2 e)
    L1.EBegin es         -> EBegin (map toL2 es)
    L1.ELocalSet x e     -> ELocalSet x (toL2 e)
    L1.EGlobalSet x e    -> EGlobalSet x (toL2 e)
    L1.EIf e1 e2 e3      -> EIf (toL2 e1) (toL2 e2) (toL2 e3)
    L1.EWhile e1 e2      -> EWhile (toL2 e1) (toL2 e2)
    L1.ELocalVar x       -> ELocalVar x
    L1.EGlobalVar x      -> EGlobalVar x
    L1.EApply f args     -> EApply (toL2 f) (map toL2 args)
    L1.EPrimApply p args -> EPrimApply p (map toL2 args)

-- rename :: L1 -> Either Text L2
-- rename = unsafePerformIO . runExceptT . flip runReaderT HashMap.empty . go where
    -- go :: L1 -> ReaderT (HashMap Text Unique) (ExceptT Text IO) L2
    -- go l1 = do
        -- env <- ask
        -- case l1 of
          -- L1.EConst k -> pure $ EConst k
          -- L1.ELet binds body -> do
              -- uniques <- replicateM (length binds) (liftIO newUnique)
              -- binds' <- zip uniques <$> traverse (go . snd) binds
              -- body' <- local (HashMap.fromList (zip (map fst binds) uniques) <>) (go body)
              -- pure $ ELet binds' body'
          -- L1.ELambda args body -> do
              -- uniques <- traverse (\x -> (x,) <$> liftIO newUnique) args
              -- ELambda (map snd uniques) <$> local (HashMap.fromList uniques <>) (go body)
          -- L1.EBegin es -> EBegin <$> traverse go es
          -- L1.ELocalSet x e
            -- | Just u <- HashMap.lookup x env -> ELocalSet u <$> go e
            -- | otherwise -> lift . throwE $ "Invalid local " <> x
          -- L1.EGlobalSet x e -> EGlobalSet x <$> go e
          -- L1.EIf a b c -> EIf <$> go a <*> go b <*> go c
          -- L1.EWhile a b -> EWhile <$> go a <*> go b
          -- L1.ELocalVar x
            -- | Just u <- HashMap.lookup x env -> pure $ ELocalVar u
            -- | otherwise -> lift . throwE $ "Invalid local " <> x
          -- L1.EGlobalVar x -> pure $ EGlobalVar x
          -- L1.EApply f args -> EApply <$> go f <*> traverse go args
          -- L1.EPrimApply p args -> EPrimApply p <$> traverse go args

detectMutable :: L2 -> HashSet Unique
detectMutable = go where
    go = \case
        EConst _ -> mempty
        ELet binds body -> foldMap (foldMap go) binds <> go body
        ELambda _ body -> go body
        EBegin es -> foldMap go es
        ELocalSet u e -> HashSet.insert u (go e)
        EGlobalSet _ e -> go e
        EIf a b c -> go a <> go b <> go c
        EWhile a b -> go a <> go b
        ELocalVar _ -> mempty
        EGlobalVar _ -> mempty
        EApply f args -> go f <> foldMap go args
        EPrimApply _ args -> foldMap go args

wrapMutable :: HashSet Unique -> L2 -> L2
wrapMutable env = go where
    go = \case
        EConst k -> EConst k
        -- (let ((x e1)
        --       (y e2))
        --   ...)
        -- ==>
        -- (let ((x (box e1))
        --       (y (box e2)))
        --   ...)
        ELet binds body -> 
            let goBind (u, e)
                  | u `HashSet.member` env = (u, EPrimApply (prim "box") [go e])
                  | otherwise              = (u, go e)
             in ELet (map goBind binds) (go body)
        -- (lambda (x y)
        --   ...)
        -- ==>
        -- (lambda (x y)
        --   (set! x (box x))
        --   (set! y (box y))
        --   ...)
        ELambda args body ->
            let f u 
                  | u `HashSet.member` env =
                      Just $ ELocalSet u (EPrimApply (prim "box") [ELocalVar u])
                  | otherwise = Nothing
                sets = mapMaybe f args
             in ELambda args $ EBegin (sets ++ [go body])
        -- x
        -- ==>
        -- (unbox x)
        ELocalVar u
          | u `HashSet.member` env -> EPrimApply (prim "unbox") [ELocalVar u]
          | otherwise -> ELocalVar u
        -- (set! x 12)
        -- ==>
        -- (set-box! x 12)
        ELocalSet u e
          | u `HashSet.member` env -> EPrimApply (prim "set-box!") [ELocalVar u, go e]
          | otherwise -> ELocalSet u (go e)
        EBegin es -> EBegin (map go es)
        EIf a b c -> EIf (go a) (go b) (go c)
        EWhile a b -> EWhile (go a) (go b)
        EApply f args -> EApply (go f) (map go args)
        EPrimApply p args -> EPrimApply p (map go args)
        EGlobalVar x -> EGlobalVar x
        EGlobalSet x e -> EGlobalSet x (go e)


