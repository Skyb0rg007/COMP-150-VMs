{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}
-- Desugar letrec and datums


-- desugar :: L0 -> L1
-- desugar = runM . cata go where
--   go :: L0F L1 -> L1

module Language.Scheme.L1
    ( L1 (..)
    , L1Constant (..)
    ) where

-- import           Control.Lens               (preview)
import           Control.Monad
-- import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
-- import           Data.Functor.Foldable.TH
-- import           Data.Bifunctor             (first)
-- import           Data.HashMap.Strict        (HashMap)
-- import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import           Data.Unique
import           Language.Scheme.L0         (L0)
import qualified Language.Scheme.L0         as L0
import           Language.Scheme.Primitives
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Class
import           Language.Scheme.Util       (tshow)

data L1Constant
    = KChar Char
    | KNum Double
    | KString Text
    | KSymbol Text
    | KEmpty
    | KBool Bool

data L1
    = EConst L1Constant
    | ELet [(Unique, L1)] L1
    | ELambda [Unique] L1
    | EBegin [L1]
    | ELocalSet Unique L1
    | EGlobalSet Text L1
    | EIf L1 L1 L1
    | EWhile L1 L1
    | ELocalVar Unique
    | EGlobalVar Text
    | EApply L1 [L1]
    | EPrimApply Prim [L1]

instance Embed L1Constant where
    embed = \case
        KBool b   -> SBool b
        KChar c   -> SChar c
        KEmpty    -> SEmpty
        KNum n    -> SNum n
        KString s -> SString s
        KSymbol s -> SSymbol s

instance Embed L1 where
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
            unique u = SSymbol $ "x" <> tshow (hashUnique u)
            embedBinds = map $ \(x, e) -> SList [unique x, embed e]

instance Project L1 where
    project = traverse desugar <=< project

desugar :: L0 -> Either Text L1
desugar = runExcept . flip evalStateT 0 . go where
    go :: L0 -> StateT Int (Except Text) L1
    go = \case
        L0.ELet binds body -> ELet <$> traverse (traverse go) binds <*> go body
        L0.ELambda args body -> ELambda args <$> go body
        L0.EBegin es -> EBegin <$> traverse go es
        L0.ELocalSet x e -> ELocalSet x <$> go e
        L0.EGlobalSet x e -> EGlobalSet x <$> go e
        L0.EIf a b c -> EIf <$> go a <*> go b <*> go c
        L0.EWhile a b -> EWhile <$> go a <*> go b
        L0.ELocalVar x -> pure $ ELocalVar x
        L0.EGlobalVar x -> pure $ EGlobalVar x
        L0.EApply a b -> EApply <$> go a <*> traverse go b
        L0.EPrimApply p args -> EPrimApply p <$> traverse go args
        L0.EDatum sexp -> pure $ datum sexp
        L0.ELetRec binds body -> do
            let namings = map (\(x, _) -> (x, EConst (KBool False))) binds
            sets <- traverse (\(x, e) -> ELocalSet x <$> go e) binds
            body' <- go body
            pure $ ELet namings $ EBegin (sets ++ [body'])

    datum :: SExp -> L1
    datum = \case
        SChar c   -> EConst (KChar c)
        SBool b   -> EConst (KBool b)
        SNum n    -> EConst (KNum n)
        SString s -> EConst (KString s)
        SSymbol s -> EConst (KSymbol s)
        SEmpty    -> EConst KEmpty
        SPair a b -> EPrimApply (prim "cons") [datum a, datum b]
        SVector _ -> error "Vector literals nyi"
        SByteVector _ -> error "Bytevector literals nyi"


