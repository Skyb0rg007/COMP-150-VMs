{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-
   Module:      Uft.Pretty
   Description: Pretty-printing AST nodes
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This typeclass corresponds to the VScheme projection functions
-}

module Uft.Pretty
    ( PrettyF (prettyF')
    , ASTStyle
    , styleKw
    , styleVar
    , styleNum
    , styleBool
    , styleChar
    , styleString
    , styleComment
    , styleSym
    , stylePrim
    , astToAnsi
    , prettyF
    , module Data.Text.Prettyprint.Doc
    , hsep'
    , vsep'
    ) where

import           Data.Text.Prettyprint.Doc hiding (SimpleDocStream (..))
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Type.OpenADT

-- | AST styling
data ASTStyle
    = StyleKeyword
    | StyleVar
    | StyleNum
    | StyleBool
    | StyleChar
    | StyleString
    | StyleComment
    | StyleSym
    | StylePrim
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Style like a keyword
styleKw :: Doc ASTStyle -> Doc ASTStyle
styleKw = annotate StyleKeyword

-- | Style like a variable
styleVar :: Doc ASTStyle -> Doc ASTStyle
styleVar = annotate StyleVar

-- | Style like a number
styleNum :: Doc ASTStyle -> Doc ASTStyle
styleNum = annotate StyleNum

-- | Style like a boolean
styleBool :: Doc ASTStyle -> Doc ASTStyle
styleBool = annotate StyleBool

-- | Style like a char
styleChar :: Doc ASTStyle -> Doc ASTStyle
styleChar = annotate StyleChar

-- | Style like a string
styleString :: Doc ASTStyle -> Doc ASTStyle
styleString = annotate StyleString

-- | Style like a comment
styleComment :: Doc ASTStyle -> Doc ASTStyle
styleComment = annotate StyleComment

-- | Style like a symbol
styleSym :: Doc ASTStyle -> Doc ASTStyle
styleSym = annotate StyleSym

-- | Style like a primitive
stylePrim :: Doc ASTStyle -> Doc ASTStyle
stylePrim = annotate StylePrim

-- | Convert the AST styling to a terminal-rendered version
astToAnsi :: ASTStyle -> AnsiStyle
astToAnsi _ = mempty

-- | Typeclass for pretty-printing
class Functor f => PrettyF f where
    prettyF' :: f (Doc ASTStyle) -> Doc ASTStyle

instance Applies '[Functor, PrettyF] r => PrettyF (Sum r) where
    prettyF' = apply @PrettyF prettyF'
    {-# INLINE prettyF' #-}

-- | Helper for folding the 'prettyF'' function
prettyF :: (Apply Functor r, Apply PrettyF r)
        => OpenADT r
        -> Doc ASTStyle
prettyF = cata prettyF'

-- | 'hsep', but for any instance of 'Foldable'
hsep' :: Foldable f => f (Doc ann) -> Doc ann
hsep' = concatWith (<+>)

-- | 'vsep', but for any instance of 'Foldable'
vsep' :: Foldable f => f (Doc ann) -> Doc ann
vsep' = concatWith (\x y -> x <> line <> y)

