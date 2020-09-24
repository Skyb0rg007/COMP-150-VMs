#!/usr/bin/env stack
-- stack script --package megaparsec --package text --resolver lts-16.15

{-# LANGUAGE OverloadedStrings #-}

import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace

type Parser = Parsec Void Text

input :: Text
input = "13.23"

main :: IO ()
main = parseTest parseNumber input

many1 :: Parser a -> Parser [a]
many1 = some

digit :: Parser Int
digit = digitToInt <$> digitChar 

parseNumber :: Parser Double
parseNumber = combine <$> sign <*> part1 <*> part2
    where
        sign :: Parser (Double -> Double)
        sign =
            (negate <$ string "-")
            <|>
            (id <$ string "+")
            <|>
            (pure id)
        part1 :: Parser Double
        part1 = foldr (\x acc -> acc * 10 + fromIntegral x) 0 <$> many1 digit
        part2 :: Parser (Maybe Double)
        part2 = optional (string "." >> foldr (\x acc -> acc / 10 + fromIntegral x / 10) 0 <$> many1 digit)
        combine f p1 p2 = f (p1 + fromMaybe 0 p2)

