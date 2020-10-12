{
-- vim: ft=haskell
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module SExpr.Parse.Lexer
    ( 
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
import           SExpr.Parse.Monad
import           SExpr.Parse.Tokens      (Token, Token_)
import qualified SExpr.Parse.Tokens      as T
}

tokens :-
    <0> $white ;

{
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
