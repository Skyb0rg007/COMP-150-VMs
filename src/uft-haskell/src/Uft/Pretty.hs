
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
    , astToAnsi
    , prettyF
    ) where

import           Data.Text (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Type.OpenADT
import           Type.VarF
import           Data.Functor.Foldable

data ASTStyle
    = StyleKeyword
    | StyleVar
    | StyleNum
    | StyleBool
    | StyleChar
    | StyleString
    | StyleComment
    | StyleSym
    deriving (Show, Eq, Ord, Enum, Bounded)

styleKw :: Doc ASTStyle -> Doc ASTStyle
styleKw = annotate StyleKeyword

styleVar :: Doc ASTStyle -> Doc ASTStyle
styleVar = annotate StyleVar

styleNum :: Doc ASTStyle -> Doc ASTStyle
styleNum = annotate StyleNum

styleBool :: Doc ASTStyle -> Doc ASTStyle
styleBool = annotate StyleBool

styleChar :: Doc ASTStyle -> Doc ASTStyle
styleChar = annotate StyleChar

styleString :: Doc ASTStyle -> Doc ASTStyle
styleString = annotate StyleString

styleComment :: Doc ASTStyle -> Doc ASTStyle
styleComment = annotate StyleComment

styleSym :: Doc ASTStyle -> Doc ASTStyle
styleSym = annotate StyleSym

astToAnsi :: ASTStyle -> AnsiStyle
astToAnsi _ = mempty

-- | Typeclass for rendering OpenADTs to the terminal
class PrettyF f where
    prettyF' :: f (Doc ASTStyle) -> Doc ASTStyle

-- | Used for 'OpenADT' instance
instance Forall r PrettyF => PrettyF (VarF r) where
    prettyF' = varFAlg @PrettyF prettyF'

-- | Helper for applying PrettyF to OpenADTs where all members satisfy the constraint
prettyF :: (Forall r Functor, Forall r PrettyF)
        => OpenADT r
        -> Doc ASTStyle
prettyF = cata prettyF'

