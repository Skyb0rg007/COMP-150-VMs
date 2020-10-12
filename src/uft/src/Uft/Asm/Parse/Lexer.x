{
-- vim: ft=haskell
{-|
   Module:      Uft.Asm.Parse.Lexer
   Description: Alex Lexer for the Uft assembly language
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Uft.Asm.Parse.Lexer
    (
    -- * Lexing tokens
      lexToken
    , lexTokens
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Data.ByteString.Internal  (w2c)
import qualified Data.ByteString.Lazy      as Lazy.ByteString
import qualified Data.ByteString.Lazy      as Lazy (ByteString)
import           Data.ByteString.Lazy.Lens (unpackedChars)
import           Data.Loc
import           Data.String.Conversions   (convertString)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Word                 (Word8)
import           Text.Read                 (readMaybe)
import           Uft.Asm.Parse.Monad
import           Uft.Asm.Parse.Tokens      (Token, Token_)
import qualified Uft.Asm.Parse.Tokens      as T
}

$ws = [\t\v\f\ ]
$cr = [\r]
$nl = [\n]
$dq = \"
-- "

@eol = $cr $nl | $nl

$labelStart = [a-zA-Z_]
$labelCont = [a-zA-Z0-9_'\?\-]
@label = $labelStart $labelCont*

$decDigit = [0-9]
@decNum = $decDigit ("_"* $decDigit)*
$hexDigit = [0-9a-fA-F]
@hexNum = $hexDigit ("_"* $hexDigit)*
$octDigit = [0-7]
@octNum = $octDigit ("_"* $octDigit)*
$binDigit = [0-1]
@binNum = $binDigit ("_"* $binDigit)*
@frac = "." @decNum
@exp = [eE] "-"? @decNum
@float = "-"? (@decNum @frac? @exp | @decNum @frac @exp?)

tokens :-
    <0> $ws+ ;
    <0> @eol { pureTok T.Newline }

    <0> @float { numTok }
    <0> "-"? "0" [xX] @hexNum { numTok }
    <0> "-"? "0" [oO] @octNum { numTok }
    <0> "-"? @decNum { numTok }
    -- TODO
    -- <0> "-"? "0" [bB] @binNum { \loc lbs -> numTok loc (lbs) }

    <0> "abs"       { pureTok T.Abs         }
    <0> "boolean?"  { pureTok T.BooleanChk  }
    <0> "car"       { pureTok T.Car         }
    <0> "cdr"       { pureTok T.Cdr         }
    <0> "cons"      { pureTok T.Cons        }
    <0> "emptylist" { pureTok T.Emptylist   }
    <0> "error"     { pureTok T.Error       }
    <0> "false"     { pureTok T.False       }
    <0> "function"  { pureTok T.Function    }
    <0> "function?" { pureTok T.FunctionChk }
    <0> "goto"      { pureTok T.Goto        }
    <0> "halt"      { pureTok T.Halt        }
    <0> "hash"      { pureTok T.Hash        }
    <0> "if"        { pureTok T.If          }
    <0> "nil"       { pureTok T.Nil         }
    <0> "nil?"      { pureTok T.NilChk      }
    <0> "null?"     { pureTok T.NullChk     }
    <0> "number?"   { pureTok T.NumberChk   }
    <0> "pair?"     { pureTok T.PairChk     }
    <0> "print"     { pureTok T.Print       }
    <0> "println"   { pureTok T.Println     }
    <0> "printu"    { pureTok T.Printu      }
    <0> "symbol?"   { pureTok T.SymbolChk   }
    <0> "true"      { pureTok T.True        }

    <0> @label { \loc lbs -> pure $ L loc (T.Label $ convertString lbs) }

    <0> ":=" { pureTok T.ColonEq   }
    <0> ":"  { pureTok T.Colon     }
    <0> "/"  { pureTok T.Div       }
    <0> "="  { pureTok T.Eq        }
    <0> ">=" { pureTok T.Geq       }
    <0> ">"  { pureTok T.Gt        }
    <0> "//" { pureTok T.IDiv      }
    <0> "{"  { pureTok T.LBrace    }
    <0> "<=" { pureTok T.Leq       }
    <0> "<"  { pureTok T.Lt        }
    <0> "("  { pureTok T.LParen    }
    -- <0> "-"  { pureTok T.Minus     }
    <0> "%"  { pureTok T.Mod       }
    <0> "*"  { pureTok T.Mul       }
    <0> "\n" { pureTok T.Newline   }
    <0> "+"  { pureTok T.Plus      }
    <0> "}"  { pureTok T.RBrace    }
    <0> ")"  { pureTok T.RParen    }
    <0> ";"  { pureTok T.Semicolon }

    <0> "%" @decNum { regTok }

    <0> $dq { startString }
    <stringState> $dq { finishString }
    <stringState> " "|\!|[\035-\091]|[\093-\126]               { \_ lbs -> addText (convertString lbs) }
    <stringState> [\192-\223]|[\128-\191]                      { \_ lbs -> addText (convertString lbs) }
    <stringState> [\224-\239][\128-\191][\128-\191]            { \_ lbs -> addText (convertString lbs) }
    <stringState> [\240-\247][\128-\191][\128-\191][\128-\191] { \_ lbs -> addText (convertString lbs) }
    <stringState> \\a { \_ _ -> addText "\a" }
    <stringState> \\b { \_ _ -> addText "\b" }
    <stringState> \\t { \_ _ -> addText "\t" }
    <stringState> \\n { \_ _ -> addText "\n" }
    <stringState> \\v { \_ _ -> addText "\v" }
    <stringState> \\f { \_ _ -> addText "\f" }
    <stringState> \\r { \_ _ -> addText "\r" }
    <stringState> \\. { \_ lbs -> addText (convertString $ Lazy.ByteString.drop 1 lbs) }

    <0> . {
        \loc lbs ->
            throwError $ "Illegal token " <> Text.pack (show lbs)
    }

{
type Action = Loc -> Lazy.ByteString -> Parser Token

-- The action does not touch the state and simply produces the given token
pureTok :: Token_ -> Action
pureTok tok = \loc _lbs -> pure (L loc tok)

-- This action parses the input as a number
numTok :: Action
numTok loc lbs =
    case readMaybe (lbs^.unpackedChars) of
      Nothing -> throwError $ "Unable to parse number! " <> Text.pack (show lbs)
      Just n -> pure $ L loc $ T.Num n

-- This action parses the input as an integer after dropping the first char
regTok :: Action
regTok loc lbs =
    case readMaybe (lbs^.unpackedChars & drop 1) of
      Nothing -> throwError $ "Unable to parse number! " <> Text.pack (show lbs)
      Just n -> pure $ L loc $ T.Reg n

-- This action sets up the parser state for string parsing
startString :: Action
startString loc lbs = do
    parserState_textBuf  .= ""
    parserState_textLeft .= locStart loc
    parserState_inText   .= True
    parserState_lexState .= stringState
    lexToken

-- This action returns the string built up in the string buffer
finishString :: Action
finishString loc lbs = do
    buf <- use parserState_textBuf
    left <- use parserState_textLeft
    parserState_textBuf  .= ""
    parserState_inText   .= False
    parserState_lexState .= 0
    pure $ L (left <--> loc) $ T.Str buf

-- This monadic action inserts the given text into the buffer
addText :: Text -> Parser Token
addText txt = do
    parserState_textBuf %= (<> txt)
    lexToken

-- | Run the lexer, producing one token of output
-- Note that this function succeeds on EOF
lexToken :: Parser Token
lexToken = do
    input <- use parserState_alexInput
    lexState <- use parserState_lexState
    case alexScan input lexState of
      AlexEOF -> do
          inText <- use parserState_inText
          when inText $
              throwError "Unclosed text constant"
          pure $ L noLoc T.EOF
      AlexError input' -> do
          throwError "AlexError!"
      AlexSkip input' _ -> do
          parserState_alexInput .= input'
          lexToken
      AlexToken input' _ action -> do
          parserState_alexInput .= input'
          let pos1 = input^.alexInput_sourcePos
              pos2 = input'^.alexInput_sourcePos
              loc = Loc pos1 pos2
              len = posCoff pos2 - posCoff pos1
              tok = Lazy.ByteString.take (fromIntegral len) (input^.alexInput_input)
          action loc tok

-- | Run the lexer until it produces EOF, returning all parsed tokens
-- This function replaces the normal pattern 'many lexToken' since
-- 'lexToken' succeeds on EOF.
lexTokens :: Parser [Token]
lexTokens = do
    t <- lexToken
    case t of
      L _ T.EOF -> pure [t]
      _         -> (t :) <$> lexTokens

-- This function is provided for the generated Alex tokenizer
-- Read the next character, updating the input state
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input =
    case Lazy.ByteString.uncons (input^.alexInput_input) of
      Nothing -> Nothing
      Just (w, rest) -> Just $ (w,) $
          input & alexInput_input .~ rest
                & alexInput_sourcePos %~ (`advancePos` w2c w)
}

