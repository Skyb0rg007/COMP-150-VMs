
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Uft.KNormal.ToVO
    ( knormToAsm
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
import qualified Uft.KNormal.Ast as KN
import qualified Uft.Asm.Ast as Asm
import           Uft.Scheme.Prims
import           Text.Read (readMaybe)
import           Uft.KNormal.ToScheme

registerNum
    :: MonadError Text m
    => Text
    -> m Int
registerNum r
  | Just ('r', rest) <- Text.uncons r =
      case readMaybe (Text.unpack rest) of
        Nothing -> throwError $ "Could not parse " <> r
        Just n -> pure n
  | otherwise = throwError $ "Could not parse " <> r

knormToAsm
    :: forall m. MonadError Text m
    => KN.Exp Text
    -> m Asm.Prog
knormToAsm = go <=< traverse registerNum
    where
        compileLit :: KN.Literal -> Asm.Literal
        compileLit = \case
            KN.LitSym s -> Asm.LitStr s
            KN.LitNum n -> Asm.LitNum n
            _ -> undefined
        compilePrim :: Prim -> (Asm.Cmd, Bool)
        compilePrim = \case
            PrimPrint -> (Asm.Print, False)
            PrimAdd -> (Asm.Add, True)
        compileLitPrim :: Prim -> Asm.LitCmd
        compileLitPrim = \case
            PrimCheck -> Asm.Check
            PrimExpect -> Asm.Expect
        go :: KN.Exp Int -> m Asm.Prog
        go = \case
            KN.ExpLet x (KN.ExpLit lit) body ->
                (Asm.LoadLit x (compileLit lit) :) <$> go body
            KN.ExpLet x (KN.ExpCmd p args) body 
              | (cmd, True) <- compilePrim p ->
                  pure [Asm.Cmd cmd (Just x) args]
              | (cmd, False) <- compilePrim p ->
                  pure [Asm.Cmd cmd Nothing args, Asm.LoadLit x Asm.LitNil]
            KN.ExpCmd p args 
              | (cmd, True) <- compilePrim p ->
                  pure [Asm.Cmd cmd (Just $ Vector.head args) (Vector.tail args)]
              | (cmd, False) <- compilePrim p ->
                  pure [Asm.Cmd cmd Nothing args]
            KN.ExpLitCmd p args lit ->
                pure [Asm.LitCmd (compileLitPrim p) (Vector.head args) (compileLit lit)]
            -- KN.ExpLit lit -> pure $ compileLit lit
            kn -> error $ show $ pretty $ knormToScheme $ fmap (Text.pack . show) kn


