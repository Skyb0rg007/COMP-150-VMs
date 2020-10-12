{-|
   Module:      Uft.Asm.Parse.Tokens
   Description: Definition of tokens used by the lexer + parser
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This module is designed to be imported qualified,
   since it uses names that clash with the Prelude.
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Uft.Asm.Parse.Tokens
    ( 
    -- * Tokens
      Token
    , Token_ (..)
    -- * Pattern synonyms used by the Happy parser
    , pattern PatLabel
    , pattern PatNum
    , pattern PatReg
    , pattern PatStr
    ) where

import           Data.Loc  (L (L))
import           Data.Text (Text)

-- | Tokens annotated with location info
type Token = L Token_

-- | The token value
data Token_
    = Abs
    | BooleanChk
    | Car
    | Cdr
    | Colon
    | ColonEq
    | Cons
    | Div
    | Emptylist
    | Eq
    | Error
    | False
    | Function
    | FunctionChk
    | Gt
    | Geq
    | Goto
    | Halt
    | Hash
    | IDiv
    | If
    | Label !Text
    | LBrace
    | Lt
    | Leq
    | LParen
    | Minus
    | Mod
    | Mul
    | Newline
    | Nil
    | NilChk
    | NullChk
    | NumberChk
    | Num !Double
    | PairChk
    | Plus
    | Print
    | Println
    | Printu
    | RBrace
    | Reg !Int
    | RParen
    | Semicolon
    | Str !Text
    | SymbolChk
    | True
    | EOF
    deriving (Show, Eq, Ord)

-- | Pattern synonym for matching located 'Label's
pattern PatLabel :: L Text -> Token
pattern PatLabel x <- (patLabel -> Just x)
    where PatLabel (L loc x) = L loc (Label x)

patLabel (L loc (Label x)) = Just $ L loc x
patLabel _ = Nothing

-- | Pattern synonym for matching located 'Num's
pattern PatNum :: L Double -> Token
pattern PatNum x <- (patNum -> Just x)
    where PatNum (L loc x) = L loc (Num x)

patNum (L loc (Num x)) = Just $ L loc x
patNum _ = Nothing

-- | Pattern synonym for matching located 'Reg's
pattern PatReg :: L Int -> Token
pattern PatReg x <- (patReg -> Just x)
    where PatReg (L loc x) = L loc (Reg x)

patReg (L loc (Reg x)) = Just $ L loc x
patReg _ = Nothing

-- | Pattern synonym for matching located 'Str's
pattern PatStr :: L Text -> Token
pattern PatStr x <- (patStr -> Just x)
    where PatStr (L loc x) = L loc (Str x)

patStr (L loc (Str x)) = Just $ L loc x
patStr _ = Nothing

