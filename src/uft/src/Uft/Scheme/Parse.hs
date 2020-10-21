
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Uft.Scheme.Parse
    ( parseScheme
    ) where

import           Control.Monad              (void)
import           Control.Monad.Except
import           Data.Bifunctor             (first)
import           Data.Char                  (chr, digitToInt)
import           Data.Foldable              (traverse_)
import           Data.Foldable              (toList)
import           Data.Functor.Foldable.TH   (makeBaseFunctor)
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HashSet
import           Data.Loc
import           Data.Maybe                 (catMaybes, fromMaybe, isJust)
import           Data.Ratio                 ((%))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Prettyprint.Doc
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector
import           Data.Void                  (Void)
import           Data.Word                  (Word8)
import           Debug.Trace
import           GHC.Float                  (int2Double)
import           Text.Megaparsec.Util
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Uft.Scheme.Ast

-- | Parser type
-- No custom error type -> Void
-- Parses 'Text'
type P = Parsec Void Text

-- | Whitespace between tokens in a line
intralineWhitespace :: P ()
intralineWhitespace = void $ oneOf [' ', '\t']

-- | The different supported line breaks
lineEnding :: P ()
lineEnding = label "line ending" $
    void (string "\r\n") <|> void (char '\r') <|> void (char '\n')

-- | Whitespace between tokens, includes linebreaks
whitespace :: P ()
whitespace = label "whitespace" $ intralineWhitespace <|> lineEnding

-- | Comments. The spec follow R7RS.
-- ; This is an EOL comment
-- #| This is an inline comment #| with nesting |# |#
-- #;(This comments out the given datum)
comment :: P ()
comment = label "comment" $
        (char ';' >> skipManyTill anySingle lineEnding)
    <|> blockComment
    <|> (string "#;" >> void schemeDatum)
    where
        blockComment = string "#|" >>
            skipManyTill (blockComment <|> void anySingle) (void $ string "|#")

-- | Spacing between tokens. Includes comments.
intertokenSpace :: P ()
intertokenSpace = skipMany (whitespace <|> comment)

-- | Consumes spacing following a token
lexeme :: P a -> P a
lexeme = L.lexeme intertokenSpace

-- | Parses a literal string, consuming trailing spaces.
symbol :: Text -> P Text
symbol = L.symbol intertokenSpace

-- | Case-insensitive 'symbol'
symbol' :: Text -> P Text
symbol' = L.symbol' intertokenSpace

-- | Helper for parsing numbers
-- Given a base and a string, return the resulting int value
numberValue :: Num a => Int -> String -> a
numberValue base =
    foldl (\x -> ((fromIntegral base * x) +) . fromIntegral . digitToInt) 0

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
inlineHexEscape = label "escape sequence" $ between (string "\\x") (char ';') $ fmap chr hexScalar

-- | Parses a scheme identifier, in accordance with R7RS
-- example-ident %-@>! anoth3r!1dent
schemeIdent :: P Text
schemeIdent = label "identifier" . lexeme . fmap Text.pack $
        (:) <$> initial <*> many subsequent
    <|> between (char '|') (char '|') (many symbolElement)
    <|> peculiarIdent
    where
        initial = letterChar <|> specialInitial
        specialInitial = oneOf ['!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~']
        subsequent = initial <|> digitChar <|> specialSubsequent
        explicitSign = oneOf ['+', '-']
        specialSubsequent = explicitSign <|> oneOf ['.', '@']
        symbolElement =
                noneOf ['\\', '|']
            <|> inlineHexEscape
            <|> mnemonicEscape
            <|> '|' <$ string "\\|"
        signSubsequent = initial <|> explicitSign <|> char '@'
        dotSubsequent = signSubsequent <|> char '.'
        peculiarIdent = do
            candidate <- peculiarIdent'
            let nums = HashSet.fromList $ ["inf.0", "nan.0"] >>= \x -> ['+':x, '-':x]
            if candidate `HashSet.member` nums
               then fail $ "\"" ++ candidate ++ "\" is not a valid identifier"
               else pure candidate
        pecularStart =
                sequence [explicitSign, signSubsequent]
            <|> sequence [explicitSign, char '.', dotSubsequent]
            <|> sequence [char '.', dotSubsequent]
        peculiarIdent' = pure <$> explicitSign
                     <|> (++) <$> pecularStart <*> many subsequent

-- | Parses a scheme boolean, in accordance with R7RS
-- #t #f #true #false
schemeBool :: P Bool
schemeBool = label "boolean" . lexeme $
        True <$ string "#t"
    <|> False <$ string "#f"
    <|> True <$ string "#true"
    <|> False <$ string "#false"

-- | Parses a scheme char, in accordance with R7RS
-- #\c #\space #\x64;
schemeChar :: P Char
schemeChar = label "character" . lexeme $
    string "#\\" >>
        characterName <|> anySingle <|> (char 'x' >> fmap chr hexScalar)
    where
        characterName =
                '\a'   <$ string "alarm"
            <|> '\b'   <$ string "backspace"
            <|> '\DEL' <$ string "delete"
            <|> '\ESC' <$ string "escape"
            <|> '\n'   <$ string "newline"
            <|> '\0'   <$ string "null"
            <|> '\r'   <$ string "return"
            <|> ' '    <$ string "space"
            <|> '\t'   <$ string "tab"

-- | Parses a scheme string, handling escape sequences
-- "this is a string"
schemeString :: P Text
schemeString = label "string" . lexeme . fmap (Text.pack . catMaybes) $
    between (char '"') (char '"') (many stringElement)
    where
        stringElement =
                Just <$> noneOf ['"', '\\']
            <|> try (Just <$> mnemonicEscape)
            <|> try (Just '"' <$ string "\\\"")
            <|> try (Just '\\' <$ string "\\\\")
            <|> try (Nothing <$ (char '\\' >> many intralineWhitespace >> lineEnding >> many intralineWhitespace))
            <|> Just <$> inlineHexEscape

-- | Parses a bytevector, a vector of bytes
-- [0, 1, 0, 1]: #u8( 0 1 0 1 )
-- [255]:        #u8( 255 )
-- []:           #u8()
schemeByteVector :: P [Word8]
schemeByteVector = label "bytevector" . lexeme $ between (symbol "#u8(") (char ')') $ many byte
    where
        byte = label "byte" . lexeme $ do
            nStr <- some digitChar
            let n = numberValue 10 nStr :: Integer
            if 0 <= n && n <= 255
               then pure $ fromInteger n
               else fail $ "byte " ++ show n ++ " is out of the range 0..255"


-- | Parses a float, syntax is inspired by R7RS
-- Binary: #b010101
-- Octal:  #o777
-- Hex:    #xdeadbeef
-- Dec:    #d10
-- Special: +inf.0 -inf.0 +nan.0 -nan.0
-- Floating (must be decimal): 3.1415
schemeNumber :: P Double
schemeNumber = label "number" . lexeme $ do
    pre <- prefix
    s <- sign
    sub <- subsequent pre
    pure $ s sub
    where
        prefix :: P Int
        prefix =
                2  <$ string' "#b"
            <|> 8  <$ string' "#o"
            <|> 16 <$ string' "#x"
            <|> 10 <$ optional (string' "#d")
        sign :: Num a => P (a -> a)
        sign = negate <$ string "-" <|> id <$ optional (string "+")
        subsequent :: Int -> P Double
        subsequent r =
                (1 / 0)  <$ string "inf.0"
            <|> (0 / 0)  <$ string "nan.0"
            <|> int2Double <$> uinteger r
            <|> if r == 10
                   then decimal
                   else fail $ "Unable to parse floating-point number with radix " ++ show r
        suffix :: P (Maybe Int)
        suffix = optional $ char 'e' >> sign <*> uinteger 10
        uinteger :: Num a => Int -> P a
        uinteger r = numberValue r <$> some (digit r)
        digit :: Int -> P Char
        digit 2 = binDigitChar
        digit 8 = octDigitChar
        digit 10 = digitChar
        digit 16 = hexDigitChar
        decimal :: P Double
        decimal = fromRational <$> choice
            [ do
                void $ char '.'
                nStr <- some (digit 10)
                let fracDigits = length nStr
                    n = numberValue 10 nStr
                s <- suffix
                case s of
                  Nothing -> pure $ n % (10 ^ fracDigits)
                  Just e  -> pure $ (n % (10 ^ fracDigits)) * (10 ^^ e)
            , try $ do
                n <- uinteger 10
                void $ char '.'
                mStr <- many (digit 10)
                let fracDigits = length mStr
                    m = numberValue 10 mStr
                s <- suffix
                case s of
                  Nothing -> pure $ n % 1 + m % (10 ^ fracDigits)
                  Just e  -> pure $ (n % 1 + m % (10 ^ fracDigits)) * (10 ^^ e)
            , do
                n <- uinteger 10 
                s <- suffix
                case s of
                  Nothing -> pure $ n % 1
                  Just e  -> pure $ (n % 1) ^^ e
            ]

-- | Parses literals, inspired by R7RS
-- Doesn't support scheme labels
schemeDatum :: P Lit
schemeDatum =
        try (LitBool <$> schemeBool)
    <|> try (LitNum <$> schemeNumber)
    -- XXX: Currently parsing chars as symbols
    <|> try (LitSym . Text.singleton <$> schemeChar)
    -- XXX: Currently parsing strings as symbols
    <|> try (LitSym <$> schemeString)
    <|> try (LitSym <$> schemeIdent)
    -- XXX: Currently parsing bytevectors as lists
    <|> try (foldl LitPair LitEmpty . map (LitNum . fromIntegral) <$> schemeByteVector)
    <|> try schemeList
    <|> schemeDotList
    <|> schemeVector
    <|> schemeAbbrev
    where
        schemeList = do
            xs <- betweenSexp $ many schemeDatum
            pure $ foldl LitPair LitEmpty xs
        schemeDotList = do
            (xs, x) <- betweenSexp $ do
                xs <- many schemeDatum
                void $ symbol "."
                x <- schemeDatum
                pure (xs, x)
            pure $ foldl LitPair x xs
        -- XXX: Currently parsing vectors as lists
        schemeVector = do
            void $ symbol "#("
            xs <- many schemeDatum
            void $ symbol ")"
            pure $ foldl LitPair LitEmpty xs
        schemeAbbrev = do
            prefix <- abbrevPrefix
            LitPair (LitSym prefix) <$> schemeDatum
        abbrevPrefix =
                "quote"            <$ char '\''
            <|> "quasiquote"       <$ char '`'
            <|> "unquote-splicing" <$ try (string ",@")
            <|> "unquote"          <$ char ','

betweenSexp :: P a -> P a
betweenSexp p = (symbol "(" *> p <* symbol ")")
            <|> (symbol "[" *> p <* symbol "]")

-- | Parses a scheme expression
schemeExpr :: P Exp
schemeExpr = label "expression" $
        ExpVar <$> schemeIdent
    <|> ExpLit <$> selfEvaluating
    <|> (char '\'' >> ExpLit <$> schemeDatum)
    <|> betweenSexp (choice
        [ label "quote" $
            symbol "quote" >> ExpLit <$> schemeDatum
        , label "lambda" $ do
            void $ symbol "lambda"
            args <- betweenSexp $ many schemeIdent
            ExpLambda (Vector.fromList args) <$> fmap Vector.fromList (some schemeExpr)
        , label "if" $
            symbol "if" >> ExpIf <$> schemeExpr <*> schemeExpr <*> optional schemeExpr
        , label "set" $
            symbol "set" >> ExpSet <$> schemeIdent <*> schemeExpr
        , label "begin" $
            symbol "begin" >> ExpBegin . Vector.fromList <$> many schemeExpr
        , label "let" $ do
            kind <- LetRec <$ symbol "letrec" <|> LetStar <$ symbol "let*" <|> Let <$ symbol "let"
            let parseBind = betweenSexp $
                    (,) <$> schemeIdent <*> schemeExpr
            binds <- betweenSexp $ many parseBind
            body <- some schemeExpr
            pure $ ExpLet kind (Vector.fromList binds) (Vector.fromList body)
        , choice [symbol "check-expect", symbol "check-assert", symbol "val"]
            >>= \name -> fail $ Text.unpack name ++ " is an invalid expression"
        , label "function call" $
            ExpApply <$> schemeExpr <*> fmap Vector.fromList (many schemeExpr)
        ])
    where
        selfEvaluating =
                try (LitBool <$> schemeBool)
            <|> try (LitNum <$> schemeNumber)
            -- XXX: Currently parsing chars as symbols
            <|> try (LitSym . Text.singleton <$> schemeChar)
            -- XXX: Currently parsing strings as symbols
            <|> try (LitSym <$> schemeString)
            <|> try (LitSym <$> schemeIdent)
            -- XXX: Currently parsing bytevectors as lists
            <|> try (foldl LitPair LitEmpty . map (LitNum . fromIntegral) <$> schemeByteVector)

-- | Parses a scheme statement
schemeStmt :: P Stmt
schemeStmt = label "statement" $
        try (betweenSexp $ choice
            [ symbol "val" >> StmtVal <$> schemeIdent <*> schemeExpr
            , symbol "check-expect" >> StmtCheckExpect <$> schemeExpr <*> schemeExpr
            , symbol "check-assert" >> StmtCheckAssert <$> schemeExpr
            ])
    <|> StmtExp <$> schemeExpr

-- | Parses a scheme program
schemeProg :: P Prog
schemeProg = label "program" $ intertokenSpace >> many schemeStmt

-- | Parses a scheme program, or produces a prettied error message
parseScheme :: MonadError Text m => FilePath -> Text -> m Prog
parseScheme file input =
    liftEither $ first (Text.pack . errorBundlePretty) $ parse schemeProg file input

