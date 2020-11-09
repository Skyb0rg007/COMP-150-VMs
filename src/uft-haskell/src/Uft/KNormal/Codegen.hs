{-# LANGUAGE UndecidableInstances #-}

module Uft.KNormal.Codegen
    ( 
    ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Deriving
import           Data.Foldable          (foldl')
import           Data.Sum
import           Data.Kind
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Primitives
import           Uft.Scheme.Ast
import           Uft.Scheme.ConvertPrim
import           Uft.Scheme.Disambiguate
import           Uft.Scheme.ListExpand
import           Uft.Util
import           Uft.KNormal.FromUnamb
import           Uft.Asm.Ast

fresh :: MonadState Int m => Text -> m Text
fresh base = do
    n <- get
    put $ n + 1
    pure $ base <> "_" <> tshow n

consec :: [Int] -> Bool
consec (x:y:xs) = x + 1 == y && consec (y:xs)
consec _ = True

class CodegenF f where
    codegenF' :: (MonadError Text m, MonadState Int m)
              => f (Maybe Int -> m [AsmInstr])
              -> Maybe Int
              -> m [AsmInstr]

instance Apply CodegenF r => CodegenF (Sum r) where
    codegenF' = apply @CodegenF codegenF'

codegenF :: ( MonadError Text m
            , MonadState Int m
            , Applies '[CodegenF, Functor] r
            )
         => OpenADT r
         -> Maybe Int
         -> m [AsmInstr]
codegenF = cata codegenF'

codegen :: (MonadError Text m, Applies '[CodegenF, Functor] r)
        => OpenADT r
        -> m [AsmInstr]
codegen x = codegenF x Nothing `evalStateT` 0

instance CodegenF (ExpPrimApplyF Int) where
    codegenF' (ExpPrimApplyF' p args) = \case
        Just r -> pure [Cmd p (V.cons r args)]
        Nothing 
          | _prim_kind p == HasEffect -> pure [Cmd p args]
          | otherwise -> pure []

instance CodegenF (ExpPrimApplyLitF Int) where
    codegenF' (ExpPrimApplyLitF' p args lit) = \case
        Just r -> pure [CmdLit p (V.cons r args) lit]
        Nothing
          | _prim_kind p == HasEffect -> pure [CmdLit p args lit]
          | otherwise -> pure []

instance CodegenF (ExpFunApplyF Int) where
    codegenF' (ExpFunApplyF' f (V.toList -> args))
      | consec (f:args) = \case
        Just r  -> pure [Cmd (prim "call") (V.fromList [r, f, last args])]
        Nothing -> pure [Cmd (prim "call") (V.fromList [last args + 1, f, last args])]
      | otherwise = error "Args not consecutive"

instance CodegenF (ExpKLetF Int) where
    codegenF' (ExpKLetF' x e1 e2) = \case
        Just r  -> liftM2 (++) (e1 (Just x)) (e2 (Just r))
        Nothing -> liftM2 (++) (e1 (Just x)) (e2 Nothing)

instance CodegenF (ExpKIfF Int) where
    codegenF' (ExpKIfF' x e1 e2) mReg = do
        l  <- fresh "L"
        l' <- fresh "L'"
        fmap mconcat $ sequence
            [ pure [Cmd (prim "if") (V.singleton x)]
            , pure [GotoLabel l]
            , e2 mReg
            , pure [GotoLabel l']
            , pure [DefLabel l]
            , e1 mReg
            , pure [DefLabel l']
            ]

instance CodegenF (ExpKWhileF Int) where
    codegenF' (ExpKWhileF' x e e') mReg = do
        l  <- fresh "L"
        l' <- fresh "L'"
        fmap mconcat $ sequence
            [ pure [GotoLabel l]
            , pure [DefLabel l']
            , e' Nothing
            , pure [DefLabel l]
            , e (Just x)
            , pure [Cmd (prim "if") (V.singleton x)]
            , pure [GotoLabel l']
            , case mReg of
                Just r  -> pure [CmdLit (prim "loadliteral") (V.singleton r) LitNil]
                Nothing -> pure []
            ]

instance CodegenF (ExpGetLocalF Int) where
    codegenF' (ExpGetLocalF' x) = \case
        Just r -> pure [Cmd (prim "copyreg") (V.fromList [r, x])]
        Nothing -> pure []

instance CodegenF (ExpSetLocalF Int) where
    codegenF' (ExpSetLocalF' x e) mReg =
        fmap mconcat $ sequence
            [ e (Just x)
            , case mReg of
                Just r  -> pure [Cmd (prim "copyreg") (V.fromList [r, x])]
                Nothing -> pure []
            ]

instance CodegenF ExpBeginF where
    codegenF' (ExpBeginF' (V.toList -> bs)) =
        case unsnoc bs of
          Nothing -> \case
            Just r  -> pure [CmdLit (prim "loadliteral") (V.singleton r) LitNil]
            Nothing -> pure []
          Just (es, e) -> \mReg -> do
              es' <- fmap mconcat $ sequence $ fmap ($ Nothing) es
              e'  <- e mReg
              pure $ es' ++ e'

instance CodegenF LitNumF where
    codegenF' (LitNumF' n) = \case
        Just r -> pure [CmdLit (prim "loadliteral") (V.singleton r) (LitNum n)]
        Nothing -> pure []
instance CodegenF LitStrF where
    codegenF' (LitStrF' n) = \case
        Just r -> pure [CmdLit (prim "loadliteral") (V.singleton r) (LitStr n)]
        Nothing -> pure []
instance CodegenF LitSymF where
    codegenF' (LitSymF' n) = \case
        Just r -> pure [CmdLit (prim "loadliteral") (V.singleton r) (LitSym n)]
        Nothing -> pure []
instance CodegenF LitCharF where
    codegenF' (LitCharF' n) = \case
        Just r -> pure [CmdLit (prim "loadliteral") (V.singleton r) (LitChar n)]
        Nothing -> pure []
instance CodegenF LitBoolF where
    codegenF' (LitBoolF' n) = \case
        Just r -> pure [CmdLit (prim "loadliteral") (V.singleton r) (LitBool n)]
        Nothing -> pure []
instance CodegenF LitEmptyF where
    codegenF' LitEmptyF' = \case
        Just r -> pure [CmdLit (prim "loadliteral") (V.singleton r) LitEmpty]
        Nothing -> pure []
instance CodegenF LitNilF where
    codegenF' LitNilF' = \case
        Just r -> pure [CmdLit (prim "loadliteral") (V.singleton r) LitNil]
        Nothing -> pure []

