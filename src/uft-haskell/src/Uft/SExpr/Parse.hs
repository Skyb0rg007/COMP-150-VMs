{-# OPTIONS_GHC -Wall #-}
{-
   Module:      Uft.SExpr.Parse
   Description: Parsing SExpressions
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   Following r7rs (for the most part), parse S-expressions.
-}

module Uft.SExpr.Parse
    ( parseSExpr
    , parseSExprs
    ) where

import           Control.Monad                      (void)
import qualified Control.Monad.Combinators.NonEmpty as NE
import           Data.Char                          (chr, digitToInt)
import           Data.Foldable                      (foldl')
import           Data.Functor                       ((<&>))
import qualified Data.HashSet                       as HashSet
import           Data.Maybe                         (catMaybes)
import           Data.Ratio                         ((%))
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Void                          (Void)
import           Data.Word                          (Word8)
import           GHC.Float                          (int2Double)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Uft.SExpr.Types
-- import           Text.Megaparsec.Debug (dbg)

-- | Parser type
-- No custom error type: 'Void'
-- Parses: 'Text'
type P = Parsec Void Text

-- | Whitespace between tokens in a line
intralineWhitespace :: P ()
intralineWhitespace = void $ oneOf (" \t" :: [Char])

-- | The different supported line breaks
lineEnding :: P ()
lineEnding = label "line ending" $
    choice [ void (string "\r\n"), void (char '\r'), void (char '\n') ]

-- | Whitespace between tokens, includes linebreaks
whitespace :: P ()
whitespace = label "whitespace" $ intralineWhitespace <|> lineEnding

-- | Comments
-- ; This is an EOL comment
-- #| This is an inline comment #| with nesting |# |#
-- #;(This comments out the given datum)
comment :: P ()
comment = label "comment" $
        (char ';' >> skipManyTill anySingle lineEnding)
    <|> blockComment
    <|> (string "#;" >> void parseDatum)
    where
        blockComment = string "#|" >>
            skipManyTill (blockComment <|> void anySingle) (void $ string "|#")

-- | Spacing between tokens. Includes comments.
intertokenSpace :: P ()
intertokenSpace = skipMany (whitespace <|> comment)

-- | Delimiter
delimiter :: P ()
delimiter = whitespace <|> void (oneOf ("|()\";" :: [Char])) <|> eof

-- | Consumes spaces following a lexeme
lexeme :: P a -> P a
lexeme p = p <* lookAhead delimiter <* intertokenSpace

-- | Parses a literal string, consuming trailing spaces.
symbol :: Text -> P Text
symbol sym = string sym <* intertokenSpace

-- | Helper for parsing numbers
-- Given a base and a string, return the resulting int value
numberValue :: Num a => Int -> String -> a
numberValue base =
    foldl' (\x -> ((fromIntegral base * x) +) . fromIntegral . digitToInt) 0

-- | Parses a hexadecimal hex integer
hexScalar :: P Int
hexScalar = label "hex scalar" $ numberValue 16 <$> some hexDigitChar

-- | Parses a c-style string escape sequence
mnemonicEscape :: P Char
mnemonicEscape = label "escape sequence" $
        '\a' <$ string "\\a"
    <|> '\b' <$ string "\\b"
    <|> '\t' <$ string "\\t"
    <|> '\n' <$ string "\\n"
    <|> '\r' <$ string "\\r"

-- | Parses a hex escape sequence
inlineHexEscape :: P Char
inlineHexEscape = label "escape sequence" $
    between (string "\\x") (char ';') $ fmap chr hexScalar

-- * Parsing atoms

-- | Parse a boolean atom
parseBool :: P Bool
parseBool = label "boolean" . lexeme $
        True  <$ string "#t"
    <|> False <$ string "#f"
    <|> True  <$ string "#true"
    <|> False <$ string "#false"

-- | Parse a character atom
parseChar :: P Char
parseChar = label "character" . lexeme $
    string "#\\" >> charName <|> anySingle <|> (char 'x' >> chr <$> hexScalar)
    where charName =
                '\a'   <$ string "alarm"
            <|> '\b'   <$ string "backspace"
            <|> '\DEL' <$ string "delete"
            <|> '\ESC' <$ string "escape"
            <|> '\n'   <$ string "newline"
            <|> '\0'   <$ string "null"
            <|> '\r'   <$ string "return"
            <|> ' '    <$ string "space"
            <|> '\t'   <$ string "tab"

-- | Parse a string atom
parseString :: P Text
parseString = label "string" . lexeme . fmap (Text.pack . catMaybes) $
    between (char '"') (char '"') $ many stringElem
    where
        stringElem :: P (Maybe Char)
        stringElem =
                Just <$> noneOf ("\"\\" :: [Char])
            <|> try (Just <$> mnemonicEscape)
            <|> try (Just '"' <$ string "\\\"")
            <|> try (Just '\\' <$ string "\\\\")
            <|> try (Nothing <$ (char '\\' >> many intralineWhitespace >> lineEnding >> many intralineWhitespace))
            <|> Just <$> inlineHexEscape

-- | Parse a bytevector atom
parseByteVector :: P [Word8]
parseByteVector = label "bytevector" . lexeme $
    between (symbol "#u8(") (char ')') $ many byte
    where
        byte = label "byte" . lexeme $ do
            nStr <- some digitChar
            let n = numberValue 10 nStr :: Integer
            if 0 <= n && n <= 255
               then pure $ fromInteger n
               else fail $ "byte " ++ show n ++ " is out of bounds for bytevectors (0..255)"

-- | Parse a number atom
parseNumber :: P Double
parseNumber = label "number" . lexeme $ choice
    [ try $ do
        pre <- prefix
        s <- sign
        sub <- subsequent pre
        notFollowedBy (alphaNumChar <|> char '.')
        pure $ s * sub
    , (1 / 0) <$ string' "+inf.0"
    , -(1 / 0) <$ string' "-inf.0"
    , (0 / 0) <$ (oneOf ("+-" :: [Char]) >> string' "nan.0")
    ]
    where
        prefix :: P Int
        prefix =
                2  <$ string' "#b"
            <|> 8  <$ string' "#o"
            <|> 16 <$ string' "#x"
            <|> 10 <$ optional (string' "#d")
        sign :: Num a => P a
        sign = (-1) <$ char '-' <|> 1 <$ optional (char '+')
        subsequent :: Int -> P Double
        subsequent 10 = try decimal <|> fmap int2Double (uinteger 10)
        subsequent r  = int2Double <$> uinteger r
        suffix :: P (Maybe Int)
        suffix = optional $ char 'e' >> (*) <$> sign <*> uinteger 10
        uinteger :: Num a => Int -> P a
        uinteger r = numberValue r <$> some (digit r)
        digit :: Int -> P Char
        digit 2  = binDigitChar
        digit 8  = octDigitChar
        digit 10 = digitChar
        digit 16 = hexDigitChar
        digit n  = error ("Invalid base: " ++ show n)
        decimal :: P Double
        decimal = fromRational <$> choice
            [ try $ do
                n <- uinteger 10
                void $ char '.'
                mStr <- many (digit 10)
                let fracDigits = length mStr
                    m = numberValue 10 mStr
                s <- suffix
                case s of
                  Nothing -> pure $ n % 1 + m % (10 ^ fracDigits)
                  Just e  -> pure $ (n % 1 + m % (10 ^ fracDigits)) * (10 ^^ e)
            , try $ do
                void $ char '.'
                nStr <- some (digit 10)
                let fracDigits = length nStr
                    n = numberValue 10 nStr
                s <- suffix
                case s of
                  Nothing -> pure $ n % (10 ^ fracDigits)
                  Just e  -> pure $ (n % (10 ^ fracDigits)) * (10 ^^ e)
            , do
                n <- uinteger 10 
                s <- suffix
                case s of
                  Nothing -> pure $ n % 1
                  Just e  -> pure $ (n % 1) ^^ e
            ]

-- | Parses a scheme identifier, in accordance with R7RS
-- example-ident %-@>! anoth3r!1dent
parseSymbol :: P Text
parseSymbol = label "identifier" . lexeme . fmap Text.pack $
        (:) <$> initial <*> many subsequent
    <|> between (char '|') (char '|') (many symbolElement)
    <|> peculiarIdent
    where
        initial = letterChar <|> specialInitial
        specialInitial = oneOf ("!$%&*/:<=>?^_~" :: [Char])
        subsequent = initial <|> digitChar <|> specialSubsequent
        explicitSign = oneOf ("+-" :: [Char])
        specialSubsequent = explicitSign <|> oneOf (".@" :: [Char])
        symbolElement =
                noneOf ("\\|" :: [Char])
            <|> inlineHexEscape
            <|> mnemonicEscape
            <|> '|' <$ string "\\|"
        signSubsequent = initial <|> explicitSign <|> char '@'
        dotSubsequent = signSubsequent <|> char '.'
        peculiarIdent = do
            candidate <- peculiarIdent'
            let nums = HashSet.fromList ["+inf.0", "+nan.0", "-inf.0", "-nan.0"]
            if candidate `HashSet.member` nums
               then fail $ "\"" ++ candidate ++ "\" is not a valid identifier"
               else pure candidate
        peculiarIdent' =
                try (concat <$> sequence [pure <$> explicitSign, pure <$> signSubsequent, many subsequent])
            <|> try (concat <$> sequence [pure <$> explicitSign, pure <$> char '.', pure <$> dotSubsequent, many subsequent])
            <|> try (pure <$> explicitSign)
            <|> (fmap concat $ sequence [pure <$> char '.', pure <$> dotSubsequent, many subsequent])

parseAtom :: P SAtom
parseAtom = label "atom" $
        SChar <$> parseChar
    <|> SBool <$> parseBool
    <|> SByteVector <$> parseByteVector
    <|> SString <$> parseString
    <|> SSymbol <$> try parseSymbol
    <|> SNum <$> parseNumber

betweenParens :: P a -> P a
betweenParens p = (symbol "(" *> p <* symbol ")")
              <|> (symbol "[" *> p <* symbol "]")

parseDatum :: P SExpr
parseDatum = label "sexpr" $ choice
    [ char '\''   >> parseDatum <&> \x -> SList [SAtom (SSymbol "quote"), x]
    , char '`'    >> parseDatum <&> \x -> SList [SAtom (SSymbol "quasiquote"), x]
    , string ",@" >> parseDatum <&> \x -> SList [SAtom (SSymbol "unquote-splicing"), x]
    , char ','    >> parseDatum <&> \x -> SList [SAtom (SSymbol "unquote"), x]
    , try (SAtom <$> label "atom" parseAtom)
    , label "list" (try schemeDotList <|> schemeList)
    , label "vector" schemeVector
    ]
    where
        schemeList = betweenParens $
            SList <$> many parseDatum
        schemeDotList = betweenParens $ do
            xs <- NE.some parseDatum
            void $ symbol "."
            x <- parseDatum
            pure $ SDotList xs x
        schemeVector = between (symbol "#(") (symbol ")") $
            SVector <$> many parseDatum

parseSExpr :: FilePath -> Text -> Either Text SExpr
parseSExpr fileName fileContent =
    case parse (intertokenSpace *> parseDatum <* eof) fileName fileContent of
      Left err -> Left $ Text.pack (errorBundlePretty err)
      Right s  -> Right s

parseSExprs :: FilePath -> Text -> Either Text [SExpr]
parseSExprs fileName fileContent =
    case parse (intertokenSpace *> many parseDatum <* eof) fileName fileContent of
      Left err -> Left $ Text.pack (errorBundlePretty err)
      Right s  -> Right s

