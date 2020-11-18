{-# LANGUAGE TemplateHaskell #-}
{-
   Module:      Language.Scheme.L0.Ast
   Description: The initial parsed language
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This datatype represents s-expressions in accordance with r7rs.
   Datum labels are not supported.
-}
module Language.Scheme.L0.Ast
    ( module Language.Scheme.L0.Ast
    ) where

import           Control.DeepSeq      (NFData)
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
import           GHC.Generics         (Generic)
import           Numeric              (showGFloat, showIntAtBase)
import           Text.Read            (Read (readPrec))
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Util

-- * Names
-- These are needed because symbols can be any sequence of characters
-- In addition, the original name of the symbol must be retained for the form (quote x)

data Name = N Text (Maybe Int)
    deriving (Show, Eq, Ord, Read, Generic, NFData, Hashable)

pattern Name :: Text -> Name
pattern Name x <- N x _
    where Name x = N x Nothing
{-# COMPLETE Name #-}

unName :: Name -> Text
unName (N base gensym) = base <> maybe "" (Text.cons '.' . tshow) gensym

instance Pretty Name where
    pretty = pretty . unName

instance IsString Name where
    fromString = Name . fromString

-- * L0: S-expressions
-- The language is first parsed as a sequence of s-expressions

type L0 =
    '[ CharF
     , StringF
     , SymbolF
     , BoolF
     , NumF
     , ByteVectorF
     , EmptyF
     , PairF
     , VectorF
     ]

newtype CharF (a :: Type) = CharF' Char
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

newtype StringF (a :: Type) = StringF' Text
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

newtype SymbolF (a :: Type) = SymbolF' Name
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

newtype BoolF (a :: Type) = BoolF' Bool
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

newtype NumF (a :: Type) = NumF' Double
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

newtype ByteVectorF (a :: Type) = ByteVectorF' [Word8]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data EmptyF (a :: Type) = EmptyF'
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data PairF a = PairF' a a
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

newtype VectorF a = VectorF' [a]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

-- * Deriving stuff
derive [deriveOpenADT, deriveShow1, deriveEq1, deriveOrd1, deriveRead1]
    [''CharF, ''StringF, ''SymbolF, ''BoolF, ''NumF, ''ByteVectorF, ''EmptyF, ''PairF, ''VectorF]

-- * Pretty-printing

instance PrettyF CharF where
    prettyF' (CharF' '\a')   = "#\\alarm"
    prettyF' (CharF' '\b')   = "#\\backspace"
    prettyF' (CharF' '\DEL') = "#\\delete"
    prettyF' (CharF' '\ESC') = "#\\escape"
    prettyF' (CharF' '\n')   = "#\\newline"
    prettyF' (CharF' '\0')   = "#\\null"
    prettyF' (CharF' '\r')   = "#\\return"
    prettyF' (CharF' ' ' )   = "#\\space"
    prettyF' (CharF' '\t')   = "#\\tab"
    prettyF' (CharF' c)
      | isPrint c = "#\\" <> pretty c
      | otherwise = "#\\x" <> pretty (showHex' (ord c) "")

instance PrettyF StringF where
    prettyF' (StringF' s) = dquotes $ pretty (Text.concatMap escape s) where
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

instance PrettyF SymbolF where
    prettyF' (SymbolF' s) = pretty $ Text.concatMap escape $ unName s
        where
            escape :: Char -> Text
            escape c
              | isAlphaNum c || c `elem` ("!$%&*/:<=>?^_~+-@." :: [Char]) = Text.singleton c
              | otherwise = "\\x" <> Text.pack (showHex' (ord c) "") <> ";"

instance PrettyF BoolF where
    prettyF' (BoolF' True)  = "#t"
    prettyF' (BoolF' False) = "#f"

instance PrettyF NumF where
    prettyF' (NumF' n)
      | isNaN n      = "+nan.0"
      | isInfinite n = if n < 0 then "-inf.0" else "+inf.0"
      | otherwise    =
          let m = round n
           in if n == fromInteger m
                 then pretty m
                 else pretty (showGFloat Nothing n "")

instance PrettyF ByteVectorF where
    prettyF' (ByteVectorF' bv) = "#u8(" <> hsep (map pretty bv) <> ")"

instance PrettyF EmptyF where
    prettyF' EmptyF' = "()"

instance PrettyF PairF where
    prettyF' (PairF' a b) = "(" <> a <+> "." <+> b <> ")"

instance PrettyF VectorF where
    prettyF' (VectorF' xs) = "#(" <> hsep xs <> ")"

pattern List' :: PairF :< r => [OpenADT r] -> OpenADT r -> OpenADT r
pattern List' xs x <- (unlist -> (xs, x))
    where List' xs x = foldr Pair x xs

pattern List :: '[PairF, EmptyF] :<: r => [OpenADT r] -> OpenADT r
pattern List xs = List' xs Empty

unlist :: PairF :< r => OpenADT r -> ([OpenADT r], OpenADT r)
unlist (Pair a b) =
    case unlist b of
      (xs, x) -> (a : xs, x)
unlist x = ([], x)

-- | Pretty-print, collapsing nested pairs
prettyL0
    :: forall r. (Applies '[Functor, PrettyF] r, '[EmptyF, PairF] :<: r)
    => OpenADT r
    -> Doc ASTStyle
prettyL0 x =
    case unlist x of
      ([], x)     -> prettyF x
      (xs, Empty) -> parens $ hsep (map prettyF xs)
      (xs, x')    -> parens $ hsep (map prettyF xs ++ [".", prettyF x'])


