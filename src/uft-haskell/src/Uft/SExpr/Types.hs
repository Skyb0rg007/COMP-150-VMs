{-# OPTIONS_GHC -Wall #-}

module Uft.SExpr.Types
    ( SExpr (SList, SDotList, SAtom, SVector, SNil, SCons)
    , SAtom (SChar, SString, SSymbol, SBool, SNum, SByteVector)
    , showSList
    , showSDotList
    , showSVector
    , showSChar
    , showSNum
    , showSString
    , showSSymbol
    , showSBool
    , showSByteVector
    ) where

import           Control.DeepSeq    (NFData)
import           Data.Char          (chr, isAlphaNum, isPrint, ord)
-- import           Data.DList         (DList)
-- import qualified Data.DList         as DList
import           Data.List.NonEmpty (NonEmpty ((:|)))
-- import qualified Data.List.NonEmpty as NE
import           Data.Monoid        (Endo (Endo, appEndo))
import           Data.String        (IsString (fromString))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Word          (Word8)
import           GHC.Generics       (Generic)
import           Numeric            (showGFloat, showIntAtBase)

-- | intToDigit, but using upper-case letters
intToDigit' :: Int -> Char
intToDigit' n
  | n >= 0  && n <= 9  = chr (ord '0' + n)
  | n >= 10 && n <= 15 = chr (ord 'A' + n)
  | otherwise = error $ "intToDigit': not a digit " ++ show n

-- | showHex, but using upper-case letters
showHex' :: (Integral a, Show a) => a -> ShowS
showHex' = showIntAtBase 16 intToDigit'
{-# SPECIALIZE showHex' :: Int -> ShowS #-}

-- | Show a char in sexpression syntax
showSChar :: Char -> ShowS
showSChar '\a'   = showString "#\\alarm"
showSChar '\b'   = showString "#\\backspace"
showSChar '\DEL' = showString "#\\delete"
showSChar '\ESC' = showString "#\\escape"
showSChar '\n'   = showString "#\\newline"
showSChar '\0'   = showString "#\\null"
showSChar '\r'   = showString "#\\return"
showSChar ' '    = showString "#\\space"
showSChar '\t'   = showString "#\\tab"
showSChar c
  | isPrint c = showString "#\\" . showChar c
  | otherwise = showString "#\\x" . showHex' (ord c)

-- | Show a number in sexpression syntax
showSNum :: Double -> ShowS
showSNum n
  | isNaN n      = showString "+nan.0"
  | isInfinite n = showString (if n < 0 then "-inf.0" else "+inf.0")
  | otherwise    =
      let m = round n
       in if n == fromInteger m
             then shows m
             else showGFloat Nothing n

-- | Show a boolean in sexpression syntax
showSBool :: Bool -> ShowS
showSBool True  = showString "#t"
showSBool False = showString "#f"

-- | Show a string in sexpression syntax
showSString :: String -> ShowS
showSString s = showChar '"' . showStringEscape escapeStrChar s . showChar '"'

-- | Show a symbol in sexpression syntax
showSSymbol :: String -> ShowS
showSSymbol = showStringEscape escapeSymChar

-- | Show a string, using a given escape function
showStringEscape :: (Char -> ShowS) -> String -> ShowS
showStringEscape f = appEndo . foldMap (Endo . f)

-- | Escape function for symbols
escapeSymChar :: Char -> ShowS
escapeSymChar c
  | isAlphaNum c || c `elem` ("!$%&*/:<=>?^_~+-@." :: [Char]) = showChar c
  | otherwise = showString "\\x" . showHex' (ord c) . showChar ';'

-- | Escape function for strings
escapeStrChar :: Char -> ShowS
escapeStrChar '\a' = showString "\\a"
escapeStrChar '\b' = showString "\\b"
escapeStrChar '\n' = showString "\\n"
escapeStrChar '\r' = showString "\\r"
escapeStrChar '\t' = showString "\\t"
escapeStrChar '\\' = showString "\\\\"
escapeStrChar c
  | isPrint c = showChar c
  | otherwise = showString "\\x" . showHex' (ord c) . showChar ';'

-- | Show a bytevector in sexpression syntax
showSByteVector :: [Word8] -> ShowS
showSByteVector [] s = "#u8()" ++ s
showSByteVector (x:xs) s = "#u8(" ++ shows x (go xs) where
    go []     = ')' : s
    go (y:ys) = ' ' : shows y (go ys)

-- | Show a list in sexpression syntax
showSList :: Show a => [a] -> ShowS
showSList [] s = "()" ++ s
showSList (x:xs) s = '(' : shows x (go xs) where
    go [] = ')' : s
    go (z:zs) = ' ' : shows z (go zs)

-- | Show a dotted list in sexpression syntax
showSDotList :: Show a => NonEmpty a -> a -> ShowS
showSDotList (x:|xs) y s = '(' : shows x (go xs) where
    go [] = " . " ++ shows y (')' : s)
    go (z:zs) = ' ' : shows z (go zs)

-- | Show a vector in sexpression syntax
showSVector :: Show a => [a] -> ShowS
showSVector xs s = '#' : showSList xs s

-- | SExpression atoms
data SAtom
    = SChar Char
    | SString Text
    | SSymbol Text
    | SBool Bool
    | SNum Double
    | SByteVector [Word8]
    deriving (Eq, Ord, Generic, NFData)

instance IsString SAtom where
    fromString = SSymbol . fromString

instance Num SAtom where
    fromInteger = SNum . fromInteger
    (+)    = error "SAtom Num instance is for literals only"
    (*)    = error "SAtom Num instance is for literals only"
    (-)    = error "SAtom Num instance is for literals only"
    abs    = error "SAtom Num instance is for literals only"
    signum = error "SAtom Num instance is for literals only"
    negate = error "SAtom Num instance is for literals only"

instance Fractional SAtom where
    fromRational = SNum . fromRational
    (/)   = error "SAtom Fractional implementation is for literals only"
    recip = error "SAtom Fractional implementation is for literals only"

instance Show SAtom where
    showsPrec _ = \case
        SChar c -> showSChar c
        SNum n -> showSNum n
        SBool b  -> showSBool b
        SString s -> showSString (Text.unpack s)
        SSymbol s -> showSSymbol (Text.unpack s)
        SByteVector bv -> showSByteVector bv

-- | Full S-expressions
data SExpr
    = SCons SExpr SExpr
    | SNil
    | SVector [SExpr]
    | SAtom SAtom
    deriving (Eq, Ord, Generic, NFData)

instance IsString SExpr where
    fromString = SAtom . fromString

instance Num SExpr where
    fromInteger = SAtom . fromInteger
    (+) = error "SAtom implementation of Num is for literals only"
    (*) = error "SAtom implementation of Num is for literals only"
    (-) = error "SAtom implementation of Num is for literals only"
    abs = error "SAtom implementation of Num is for literals only"
    signum = error "SAtom implementation of Num is for literals only"
    negate = error "SAtom implementation of Num is for literals only"

instance Fractional SExpr where
    fromRational = SAtom . fromRational
    (/) = error "SAtom implementation of Fractional is for literals only"
    recip = error "SAtom implementation of Fractional is for literals only"

instance Show SExpr where
    showsPrec _  = \case
        SAtom a -> shows a
        SList xs -> showSList xs
        SDotList xs x -> showSDotList xs x
        SVector xs -> showSVector xs
        _ -> error "Shouldn't match"

-- | Pattern that matches well-formed lists
pattern SList :: [SExpr] -> SExpr
pattern SList xs <- (unslist -> Just xs)
    where SList xs = foldr SCons SNil xs

-- | Pattern that matches ill-formed lists
-- Note that this produces well-formed lists when applied to 'SNil'
pattern SDotList :: NonEmpty SExpr -> SExpr -> SExpr
pattern SDotList xs x <- (unsdotlist -> Just (xs, x))
    where SDotList xs x = foldr SCons x xs
-- No COMPLETE pragma since it causes overlapping pattern warnings
-- {-# COMPLETE SList, SDotList, SVector, SAtom #-}

unslist :: SExpr -> Maybe [SExpr]
unslist SNil = Just []
unslist (SCons x xs) =
    case unslist xs of
      Nothing -> Nothing
      Just xs' -> Just (x : xs')
unslist _ = Nothing

unsdotlist :: SExpr -> Maybe (NonEmpty SExpr, SExpr)
unsdotlist (unsdotlist' -> Just (x:xs, y)) = Just (x:|xs, y)
unsdotlist _ = Nothing

unsdotlist' :: SExpr -> Maybe ([SExpr], SExpr)
unsdotlist' SNil = Nothing
unsdotlist' (SCons x xs) =
    case unsdotlist' xs of
      Nothing -> Nothing
      Just (xs', x') -> Just (x : xs', x')
unsdotlist' x = Just ([], x)

