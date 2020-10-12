{-|
   Module:      Uft.Asm.ToVO
   Description: Convert things to Virtual Object code
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Uft.Asm.ToVO
    ( ToVO (toVO)
    ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor         ((<&>))
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Uft.Asm.Ast
import qualified Data.Vector as Vector

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Class for converting elements to Virtual Object code
class ToVO a where
    -- | Convert to VO code. This operation may fail.
    toVO :: a -> Either Text Text

instance ToVO Prog where
    toVO prog = do
        prog' <- traverse toVO prog
        let len = length prog'
        pure . Text.unlines $ ".load module " <> tshow len : prog'

instance ToVO Instr where
    toVO = \case
        GotoOffset offset -> pure $ "goto " <> tshow offset
        LoadLit r lit -> toVO lit <&> \l -> "loadliteral " <> tshow r <> " " <> l
        LoadFunc r arity body -> do
            body' <- traverse toVO body
            let len = length body'
            pure . Text.unlines $
                [".load " <> tshow r <> " function " <> tshow arity <> " " <> tshow len] ++ body'
        LitCmd cmd r lit -> do
            lit' <- toVO lit
            cmd' <- toVO cmd
            pure . Text.unwords $ [cmd', tshow r, lit']
        Cmd cmd (Just r) (Vector.toList -> args) ->
            (<>) <$> toVO cmd <*> pure (" " <> Text.unwords (map tshow (r:args)))
        Cmd cmd Nothing  (Vector.toList -> args) ->
            (<>) <$> toVO cmd <*> pure (" " <> Text.unwords (map tshow args))
        instr -> Left $ "Unable to translate " <> tshow instr

instance ToVO Literal where
    toVO = \case
        LitNum n     -> pure (tshow n)
        LitStr s     -> pure (tshow s)
        LitTrue      -> pure "true"
        LitFalse     -> pure "false"
        LitEmptylist -> pure "emptylist"
        LitNil       -> pure "nil"

instance ToVO LitCmd where
    toVO = pure . \case
        Check     -> "check"
        Expect    -> "expect"
        SetGlobal -> "setglobal"
        GetGlobal -> "getglobal"

instance ToVO Cmd where
    toVO = pure . \case
        Abs         -> "abs"
        BooleanChk  -> "boolean?"
        Car         -> "car"
        Cdr         -> "cdr"
        Cons        -> "cons"
        Div         -> "div"
        Eq          -> "eq"
        FunctionChk -> "function?"
        Gt          -> "gt"
        Geq         -> "geq"
        Halt        -> "halt"
        Hash        -> "hash"
        IDiv        -> "idiv"
        If          -> "if"
        Lt          -> "lt"
        Leq         -> "leq"
        Minus       -> "minus"
        Mod         -> "mod"
        Mul         -> "mul"
        NilChk      -> "nil?"
        NullChk     -> "null?"
        NumberChk   -> "number?"
        PairChk     -> "pair?"
        Plus        -> "plus"
        Print       -> "print"
        Println     -> "println"
        Printu      -> "printu"
        SymbolChk   -> "symbol?"
