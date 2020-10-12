{-|
   Module:      Uft.Asm.Ast
   Description: Abstract syntax for the Uft assembly language
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Uft.Asm.Ast
    ( 
    -- * AST
      Prog
    , Instr (..)
    , Literal (..)
    , Cmd (..)
    -- * Helpers
    , commandArity
    , commandInfix
    , commandReturnVal
    , validateInstr
    ) where

import           Control.Monad.Except
import           Data.Foldable        (traverse_)
import           Data.Maybe           (isJust)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector

-- | A Uft assembly program is a list of instructions
type Prog = [Instr]

-- | A single Uft assembly instruction
data Instr
    = Deflabel !Text -- lbl:
    | GotoLabel !Text -- goto lbl
    | GotoOffset !Int -- goto n
    | LoadLit !Int !Literal -- %1 := <lit>
    | LoadFunc !Int !Int ![Instr] -- %1 := function n { <instr>* }
    | Cmd !Cmd !(Maybe Int) !(Vector Int) -- [%1 :=] <cmd> <reg>*
    deriving (Show, Eq, Ord)

-- | Enumeration of all the possible assembly commands
data Cmd
    = Abs
    | BooleanChk
    | Car
    | Cdr
    | Cons
    | Div
    | Eq
    | FunctionChk
    | Gt
    | Geq
    | Halt
    | Hash
    | IDiv
    | If
    | Lt
    | Leq
    | Minus
    | Mod
    | Mul
    | NilChk
    | NullChk
    | NumberChk
    | PairChk
    | Plus
    | Print
    | Println
    | Printu
    | SymbolChk
    deriving (Show, Eq, Ord, Enum)

-- | Literals
data Literal
    = LitNum !Double
    | LitStr !Text
    | LitTrue
    | LitFalse
    | LitEmptylist
    | LitNil
    deriving (Show, Eq, Ord)

-- | The number of register arguments a command takes
commandArity :: Cmd -> Int
commandArity = \case
    Abs         -> 1
    BooleanChk  -> 1
    Car         -> 1
    Cdr         -> 1
    Cons        -> 2
    Div         -> 2
    Eq          -> 2
    FunctionChk -> 1
    Gt          -> 2
    Geq         -> 2
    Halt        -> 0
    Hash        -> 1
    IDiv        -> 2
    If          -> 1
    Lt          -> 2
    Leq         -> 2
    Minus       -> 2
    Mod         -> 2
    Mul         -> 2
    NilChk      -> 1
    NullChk     -> 1
    NumberChk   -> 1
    PairChk     -> 1
    Plus        -> 2
    Print       -> 1
    Println     -> 1
    Printu      -> 1
    SymbolChk   -> 1
    
-- | True if the given command is written infix
commandInfix :: Cmd -> Bool
commandInfix cmd = commandArity cmd == 2 && cmd /= Cons

-- | True if the given command uses a return register
commandReturnVal :: Cmd -> Bool
commandReturnVal = \case
    Halt    -> False
    If      -> False
    Print   -> False
    Println -> False
    Printu  -> False
    _       -> True

-- | Errors out with a message if the given instruction is ill-formed
validateInstr :: MonadError Text m => Instr -> m ()
validateInstr = \case
    Deflabel "" -> throwError "Empty string in Deflabel!"
    Deflabel _ -> pure ()
    GotoLabel "" -> throwError "Empty string in GotoLabel!"
    GotoLabel _ -> pure ()
    GotoOffset _ -> pure ()
    LoadLit n _
      | n < 0 -> throwError "Negative register"
      | otherwise -> pure ()
    LoadFunc n arity instrs
      | n < 0 -> throwError "Negative register"
      | arity < 0 -> throwError "Negative arity"
      | otherwise -> traverse_ validateInstr instrs
    Cmd cmd ret args
      | Just n <- ret, n < 0 -> throwError "Negative register"
      | Vector.any (< 0) args -> throwError "Negative register"
      | commandArity cmd /= Vector.length args -> throwError $ "Wrong number of args to " <> Text.pack (show cmd)
      | isJust ret /= commandReturnVal cmd -> throwError $ "Mismatch of return value provided " <> Text.pack (show cmd)
      | otherwise -> pure ()
{-# SPECIALIZE validateInstr :: Instr -> Either Text () #-}

