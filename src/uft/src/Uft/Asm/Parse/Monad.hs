{-|
   Module:      Uft.Asm.Parse.Monad
   Description: Monad used for parsing + lexing assembly
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   While Alex includes pre-defined monads for lexing+parsing,
   writing my own was not too bad since I had done so for a previous
   project as well.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Uft.Asm.Parse.Monad
    ( AlexInput (..)
    , alexInput_sourcePos
    , alexInput_input
    , ParserState (..)
    , parserState_textBuf
    , parserState_textLeft
    , parserState_inText
    , parserState_alexInput
    , parserState_lexState
    , Parser
    , runParser
    ) where

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.Loc
import           Data.Text            (Text)

-- | The parser's input state
data AlexInput = AlexInput
    { _alexInput_sourcePos :: !Pos -- ^ Where we are in the source
    , _alexInput_input     :: !Lazy.ByteString -- ^ The rest of the source
    }
    deriving (Show)

-- | The entire parser state
-- Includes information about lex state and text parsing
data ParserState = ParserState
    { _parserState_textBuf   :: !Text -- ^ Used to gather text for lexing strings
    , _parserState_textLeft  :: !Loc  -- ^ The location of the left quote when lexing strings. Used for error reporting.
    , _parserState_inText    :: !Bool -- ^ True if the lexer is currently lexing a string
    , _parserState_alexInput :: !AlexInput -- ^ The input state, used by Alex
    , _parserState_lexState  :: !Int -- ^ The current lex state
    }
    deriving (Show)

-- | The lexer + parser monad
newtype Parser a = P (StateT ParserState (Except Text) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError Text
             , MonadState ParserState
             )

-- | How to run the parser monad
runParser
    :: Parser a        -- ^ The parser to run
    -> Pos             -- ^ The starting position (normally created via 'startPos')
    -> Lazy.ByteString -- ^ The input string
    -> Either Text a   -- ^ Left on error
runParser (P m) pos input = runExcept $ m `evalStateT` pState
    where
        pState = ParserState "" noLoc False (AlexInput pos input) 0

makeLenses ''AlexInput
makeLenses ''ParserState
