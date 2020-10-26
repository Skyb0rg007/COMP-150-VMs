{-# LANGUAGE UndecidableInstances #-}
{-
   Module:      Uft.Scheme.Disambiguate
   Description: Transform instance for Scheme -> UnambiguousScheme
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.Scheme.Disambiguate () where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable                         (foldrM)
import           Data.HashSet                          (HashSet)
import qualified Data.HashSet                          as HashSet
import           Data.Singletons                       (sing)
import           Data.Text                             (Text)
import           Data.Functor ((<&>))
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Vector                           (Vector)
import qualified Data.Vector                           as Vector
import           Uft.AstNodes
import           Uft.Primitives
import qualified Uft.Scheme.Ast                        as In
import qualified Uft.UnambiguousScheme.Ast             as Out
import           Data.Functor.Foldable
import           Uft.Util
import           Uft.Transform

-- Used to ensure all primitives are fully saturated
etaExpand
    :: SomePrimitive
    -> Out.Exp
etaExpand (SomePrimitive (p@Primitive{} :: Primitive a b)) =
    ExpLambda argsVec $ ExpApplyPrim (SomePrim p args ret)
        where
        argsVec :: Vector Text
        args :: PrimVec a Out.Exp
        (args, argsVec) =
            case sing @a of
              SPNullary -> (PVNullary, Vector.empty)
              SPUnary   -> (PVUnary (ExpVarLocal "a"), Vector.singleton "a")
              SPBinary  -> ( PVBinary (ExpVarLocal "a") (ExpVarLocal "b")
                           , Vector.fromList ["a", "b"] )
        ret :: PrimRet b ()
        ret = case sing @b of
                SPRet   -> PRSome ()
                SPNoRet -> PRNone

instance Transform "disambiguate" where
    type From "disambiguate" = In.Prog
    type To "disambiguate" = Out.Prog
    transform' _ = disambiguate

disambiguate
    :: MonadError Text m
    => In.Prog
    -> m Out.Prog
disambiguate = traverse disambiguateStmt

disambiguateStmt
    :: forall m. (MonadError Text m)
    => In.Stmt
    -> m Out.Stmt
disambiguateStmt = \case
    StmtVal x e -> StmtVal x <$> disambiguateExp e
    StmtDefine x args e -> StmtDefine x args <$> disambiguateExp e
    StmtCheckExpect e1 t1 e2 t2 ->
        StmtCheckExpect <$> disambiguateExp e1 <*> pure t1 <*>
            disambiguateExp e2 <*> pure t2
    StmtCheckAssert e t -> StmtCheckAssert <$> disambiguateExp e <*> pure t
    StmtExp e -> StmtExp <$> disambiguateExp e

disambiguateExp
    :: MonadError Text m
    => In.Exp
    -> m Out.Exp
disambiguateExp e = cata alg e `runReaderT` HashSet.empty where
    alg :: forall m. (MonadError Text m, MonadReader (HashSet Text) m)
        => In.ExpF (m Out.Exp)
        -> m Out.Exp
    alg = \case
        ExpLitF lit -> pure $ disambiguateLit lit
        ExpVarF x
          | Just p <- parsePrimitive x -> pure $ etaExpand p
          | otherwise -> asks (HashSet.member x) <&> \case
            True -> ExpVarLocal x
            False -> ExpVarGlobal x
        ExpSetF x e -> asks (HashSet.member x) >>= \case
            True -> ExpSetLocal x <$> e
            False -> ExpSetGlobal x <$> e
        ExpIfF e1 e2 e3 -> ExpIf <$> e1 <*> e2 <*> e3
        ExpWhileF e1 e2 -> ExpWhile <$> e1 <*> e2
        ExpBeginF es -> ExpBegin <$> sequence es
        ExpLetF In.LKLet binds body -> do
            let names = hashSetFromFoldable (fmap fst binds)
            binds' <- traverse sequence binds
            ExpLet Out.LKLet binds' <$> local (names <>) body
        ExpLetF In.LKLetRec binds body -> do
            let names = hashSetFromFoldable (fmap fst binds)
            binds' <- local (names <>) $ traverse sequence binds
            ExpLet Out.LKLetRec binds' <$> local (names <>) body
        ExpLetF In.LKLetStar binds body -> do
            let f :: (Text, m Out.Exp) -> m Out.Exp -> m Out.Exp
                f (name, e) rest = do
                    e' <- e
                    ExpLet Out.LKLet (Vector.singleton (name, e')) <$>
                        local (HashSet.insert name) rest
            foldr f body binds
        ExpApplyF f args -> ExpApply <$> f <*> sequence args
        ExpLambdaF args e ->
            let names = hashSetFromFoldable args
             in ExpLambda args <$> local (names <>) e
        _ -> undefined

disambiguateLit :: In.Lit -> Out.Exp
disambiguateLit = cata alg where
    alg :: In.LitF Out.Exp -> Out.Exp
    alg = \case
        LitNumF n    -> ExpLit $ LitNum n
        LitStrF str  -> ExpLit $ LitStr str
        LitSymF sym  -> ExpLit $ LitSym sym
        LitCharF c   -> ExpLit $ LitChar c
        LitBoolF b   -> ExpLit $ LitBool b
        LitEmptyF    -> ExpLit LitEmpty
        LitPairF a b -> ExpApplyPrim $
            SomePrim primCons (PVBinary a b) (PRSome ())
        LitVectorF _ -> error "Vectors are NYI"
        LitByteVecF _ -> error "Byte vectors are NYI"

