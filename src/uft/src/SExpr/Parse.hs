
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SExpr.Parse
    ( 
    ) where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           SExpr.Ast
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineComment blockComment
    where
        lineComment = L.skipLineComment ";"
        blockComment = L.skipBlockCommentNested "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser Text
symbol' = L.symbol' sc

parseSExpr
    :: Parser atom
    -> Map Char (SExpr atom -> SExpr atom)
    -> Parser (SExpr atom)
parseSExpr atom reader = do
    let sExpr = label "s-expr" $ parseSExpr atom reader
    sc
    s <- lookAhead $ observing anySingle
    ret <- case s of
             Left _ -> fail "Unexpected EOF"
             Right '(' ->
                 anySingle >> sc >> parseList sExpr
             Right c
               | Just r <- Map.lookup c reader -> anySingle >> fmap r sExpr
             _ -> SAtom <$> atom
    sc
    pure ret

parseList
    :: Parser (SExpr atom)
    -> Parser (SExpr atom)
parseList sExpr = do
    s <- lookAhead $ observing anySingle
    case s of
      Left _ -> fail "Unexpected EOF"
      Right ')' -> anySingle >> pure SNil
      _ -> do
          car <- sExpr
          sc
          lookAhead (observing anySingle) >>= \case
            Right '.' -> do
                anySingle
                cdr <- sExpr
                sc
                _ <- single ')'
                sc
                pure $ SCons car cdr
            Right ')' -> do
                anySingle
                sc
                pure $ SCons car SNil
            _ -> do
                cdr <- parseList sExpr
                pure $ SCons car cdr
