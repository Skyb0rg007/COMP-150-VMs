{-# LANGUAGE TemplateHaskell #-}
{-
   Module:      Language.Scheme.SExp.Ast
   Description: The initial parsed language
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This datatype represents s-expressions in accordance with r7rs.
   Datum labels are not supported.
-}
module Language.Scheme.SExp.Ast
    ( module Language.Scheme.SExp.Ast
    ) where

import           Control.DeepSeq      (NFData)
import           Control.Lens.TH      (makePrisms)
import           Data.Char            (chr, isAlphaNum, isPrint, ord)
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Hashable        (Hashable)
import           Data.Kind            (Type)
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import           Data.Monoid          (Endo (Endo, appEndo))
import           Data.String          (IsString (fromString))
import           Data.Text            (Text)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Word            (Word8)
import           GHC.Exts             (IsList (..))
import           GHC.Generics         (Generic)
-- import           Language.Scheme.Name
import           Numeric              (showGFloat, showIntAtBase)
import           Text.Read            (Read (readPrec))
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Util

-- * S-expressions
-- The language is first parsed as a sequence of s-expressions

data SExp
    = SChar Char
    | SString Text
    | SSymbol Text
    | SBool Bool
    | SNum Double
    | SByteVector [Word8]
    | SEmpty
    | SPair SExp SExp
    | SVector [SExp]
    deriving (Eq, Ord)

-- * Pretty-printing

prettySChar :: Char -> Doc ann
prettySChar = \case
    '\a'   -> "#\\alarm"
    '\b'   -> "#\\backspace"
    '\DEL' -> "#\\delete"
    '\ESC' -> "#\\escape"
    '\n'   -> "#\\newline"
    '\0'   -> "#\\null"
    '\r'   -> "#\\return"
    ' '    -> "#\\space"
    '\t'   -> "#\\tab"
    c | isPrint c -> "#\\" <> pretty c
      | otherwise -> "#\\x" <> pretty (showHex' (ord c) "")

prettySString :: Text -> Doc ann
prettySString s = dquotes $ pretty (Text.concatMap escape s) where
    escape :: Char -> Text
    escape '\a' = "\\a"
    escape '\b' = "\\b"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape '\\' = "\\\\"
    escape c
      | isPrint c = Text.singleton c
      | otherwise = "\\x" <> Text.pack (showHex' (ord c) "") <> ";"

prettySSymbol :: Text -> Doc ann
prettySSymbol s = pretty $ Text.concatMap escape s where
    escape :: Char -> Text
    escape c
      | isAlphaNum c || c `elem` ("!$%&*/:<=>?^_~+-@." :: [Char]) = Text.singleton c
      | otherwise = "\\x" <> Text.pack (showHex' (ord c) "") <> ";"

prettySBool :: Bool -> Doc ann
prettySBool True  = "#t"
prettySBool False = "#f"

prettySNum :: Double -> Doc ann
prettySNum n
  | isNaN n      = "+nan.0"
  | isInfinite n = if n < 0 then "-inf.0" else "+inf.0"
  | otherwise    =
      let m = round n
       in if n == fromInteger m
             then pretty m
             else pretty (showGFloat Nothing n "")

prettySByteVector :: [Word8] -> Doc ann
prettySByteVector bv = "#u8(" <> hsep (map pretty bv) <> ")"

prettySVector :: [Doc ann] -> Doc ann
prettySVector xs = "#(" <> hsep xs <> ")"

pattern SList' :: [SExp] -> SExp -> SExp
pattern SList' xs x <- (unlist -> (xs, x))
    where SList' xs x = foldr SPair x xs

pattern SList :: [SExp] -> SExp
pattern SList xs = SList' xs SEmpty

unlist :: SExp -> ([SExp], SExp)
unlist (SPair a b) =
    case unlist b of
      (xs, x) -> (a : xs, x)
unlist x = ([], x)

instance IsString SExp where
    fromString = SSymbol . fromString

instance IsList SExp where
    type Item SExp = SExp
    fromList = SList
    toList (SList es) = es
    toList _ = error "Attempt to match a non-list"

instance Pretty SExp where
    pretty = \case
        SChar c        -> prettySChar c
        SString s      -> prettySString s
        SSymbol s      -> prettySSymbol s
        SBool b        -> prettySBool b
        SNum n         -> prettySNum n
        SByteVector bv -> prettySByteVector bv
        SVector v      -> prettySVector (map pretty v)
        SList ["quote", x]            -> "'"  <> pretty x
        SList ["quasiquote", x]       -> "`"  <> pretty x
        SList ["unquote", x]          -> ","  <> pretty x
        SList ["unquote-splicing", x] -> ",@" <> pretty x
        SList es       -> parens $ hsep (map pretty es)
        SList' es e    -> parens $ hsep (map pretty es) <+> "." <+> pretty e

makePrisms ''SExp

