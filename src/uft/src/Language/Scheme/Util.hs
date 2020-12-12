{-# OPTIONS_GHC -Wall #-}
{-
   Module:      Language.Scheme.Util
   Description: Miscellaneous utilities used throughout Uft code
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Language.Scheme.Util
    ( 
    -- * Pretty printing
      prettyText
    , prettyLText
    , renderText
    -- * Text utilities
    , tshow
    -- * Miscellaneous
    , foldMapM
    , derive
    , unsnoc
    , hashSetFromFoldable
    , showHex'
    ) where

import           Data.Char                             (chr, ord)
import           Data.Foldable                         (foldlM)
import           Data.Hashable                         (Hashable)
import           Data.HashSet                          (HashSet)
import qualified Data.HashSet                          as HashSet
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.Lazy                        as Lazy (Text)
import           Data.Text.Prettyprint.Doc             (Pretty (pretty))
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import           Numeric                               (showIntAtBase)

-- | Render a 'Pretty' value into 'Text'
prettyText :: Pretty a => a -> Text
prettyText = Pretty.Text.renderStrict . Pretty.layoutPretty opts . pretty
    where opts = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 40 1 }

renderText :: Pretty.Doc a -> Text
renderText = Pretty.Text.renderStrict . Pretty.layoutPretty opts
    where opts = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 40 1 }

-- | Render a 'Pretty' value into 'Lazy.Text'
prettyLText :: Pretty a => a -> Lazy.Text
prettyLText = Pretty.Text.renderLazy . Pretty.layoutPretty opts . pretty
    where opts = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.AvailablePerLine 40 1 }

-- | Convert a 'Show'-able value into 'Text'
tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Monadic version of 'foldMap'
-- Implemented using a strict left fold
foldMapM :: (Monad m, Monoid w, Foldable t)
         => (a -> m w)
         -> t a
         -> m w
foldMapM f = foldlM go mempty
    where go acc a = f a >>= \w -> pure $! acc <> w

-- | Helper for deriving multiple classes on multiple types
-- derive [deriveEq1, deriveOrd1] [''Ty1, ''Ty2]
derive :: Applicative f => [a -> f [b]] -> [a] -> f [b]
derive ders names = concat <$> sequenceA [der name | der <- ders, name <- names]

-- | Uncons, but in reverse
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (x:xs) =
    case unsnoc xs of
      Nothing -> Just ([], x)
      Just (a, b) -> Just (x:a, b)

hashSetFromFoldable
    :: (Hashable a, Eq a, Foldable f)
    => f a
    -> HashSet a
hashSetFromFoldable = foldr HashSet.insert HashSet.empty

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


