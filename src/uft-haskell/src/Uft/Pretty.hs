{-# OPTIONS_GHC -Wall #-}
{-
   Module:      Uft.Pretty
   Description: Pretty-printing AST nodes
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
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
    , astToAnsi
    , prettyF
    ) where

import           Data.Char                                 (isPrint, ord)
import           Data.Functor.Foldable                     (cata)
import           Data.Text                                 (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Vector                               as Vector
import           Type.OpenADT
import           Type.VarF
import           Uft.AstNodes
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)

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

-- | Convert the AST styling to a terminal-rendered version
astToAnsi :: ASTStyle -> AnsiStyle
astToAnsi _ = mempty

-- | Typeclass for pretty-printing
class PrettyF f where
    prettyF' :: f (Doc ASTStyle) -> Doc ASTStyle

instance Forall r PrettyF => PrettyF (VarF r) where
    prettyF' = varFAlg @PrettyF prettyF'

-- | Helper for folding the 'prettyF'' function
prettyF :: (Forall r Functor, Forall r PrettyF)
        => OpenADT r
        -> Doc ASTStyle
prettyF = cata prettyF'

-- * Pretty-printing instances

instance PrettyF LitNumF where
    prettyF' (LitNumF' n) = styleNum (pretty n)
instance PrettyF LitStrF where
    prettyF' (LitStrF' s) = styleString (dquotes (pretty s))
instance PrettyF LitSymF where
    prettyF' (LitSymF' s) = styleSym (pretty s)
instance PrettyF LitCharF where
    prettyF' (LitCharF' c) 
      | isPrint c = styleChar ("#\\" <> pretty c)
      | otherwise = styleChar ("#\\x" <> pretty (ord c))
instance PrettyF LitBoolF where
    prettyF' (LitBoolF' b) = styleBool (if b then "#t" else "#f")
instance PrettyF LitEmptyF where
    prettyF' LitEmptyF' = "()"
instance PrettyF LitPairF where
    prettyF' (LitPairF' a b) = parens $ a <+> "." <+> b
instance PrettyF LitVectorF where
    prettyF' (LitVectorF' v) = "#(" <> hsep (Vector.toList v) <> ")"
instance PrettyF LitByteVecF where
    prettyF' (LitByteVecF' v) = "#u8(" <> hsep (map pretty (Vector.toList v)) <> ")"

class (Functor f, PrettyF f) => PrettyLitF f where
    requiresQuoting :: f a -> Bool
    requiresQuoting _ = True
    prettyLitF'
        :: f (Seq (Doc ASTStyle) -> Doc ASTStyle)
        -> Seq (Doc ASTStyle)
        -> Doc ASTStyle
    prettyLitF' x = \case
        Seq.Empty -> x'
        cont -> parens $ hsep' $ cont <> Seq.fromList [".", x']
        where x' = prettyF' $ fmap ($ Seq.Empty) x
              hsep' = concatWith (<+>)

instance (Forall r PrettyF, Forall r PrettyLitF, Forall r Functor) => PrettyLitF (VarF r) where
    prettyLitF' = varFAlg @PrettyLitF prettyLitF'

instance PrettyLitF LitSymF
instance PrettyLitF LitVectorF
instance PrettyLitF LitByteVecF where requiresQuoting _ = False
instance PrettyLitF LitNumF where requiresQuoting _ = False
instance PrettyLitF LitStrF where requiresQuoting _ = False
instance PrettyLitF LitCharF where requiresQuoting _ = False
instance PrettyLitF LitBoolF where requiresQuoting _ = False
instance PrettyLitF LitEmptyF where prettyLitF' LitEmptyF' xs = parens $ concatWith (<+>) xs
instance PrettyLitF LitPairF where prettyLitF' (LitPairF' a b) xs = b (xs Seq.:|> a Seq.Empty)

instance (Forall lit Functor, Forall lit PrettyF, Forall lit PrettyLitF)
  => PrettyF (ExpLitF (OpenADT lit)) where
    prettyF' (ExpLitF' lit) =
        (if requiresQuoting (unfix lit) then squote else "")
        <> cata prettyLitF' lit Seq.Empty
instance Pretty name => PrettyF (ExpVarF name) where
    prettyF' (ExpVarF' x) = pretty x
instance Pretty name => PrettyF (ExpSetF name) where
    prettyF' (ExpSetF' x e) = parens $
        styleKw "set" <+> styleVar (pretty x) <+> align e
instance Pretty name => PrettyF (ExpSetLocalF name) where
    prettyF' (ExpSetLocalF' x e) = parens $
        styleKw "set" <+> styleVar (pretty x) <+> align e
instance PrettyF ExpSetGlobalF where
    prettyF' (ExpSetGlobalF' x e) = parens $
        styleKw "set" <+> styleVar (pretty x) <+> align e
instance PrettyF ExpIfF where
    prettyF' (ExpIfF' cond t f) = parens $
        flatAlt
        (vsep [styleKw "if" <+> cond, indent 2 $ vsep [t, f]])
        (styleKw "if" <+> cond <+> t <+> f)
instance PrettyF ExpWhileF where
    prettyF' (ExpWhileF' cond body) = parens $
        flatAlt
        (vsep [styleKw "while" <+> cond, indent 2 body])
        (styleKw "while" <+> cond <+> body)
instance PrettyF ExpBeginF where
    prettyF' (ExpBeginF' (Vector.toList -> es)) = parens $
        flatAlt
        (vsep [styleKw "begin", indent 2 $ vsep es])
        (hsep $ styleKw "begin" : es)
instance PrettyF ExpApplyF where
    prettyF' (ExpApplyF' f (Vector.toList -> xs)) =
        case xs of
          [] -> parens f
          arg:args -> parens $
              align $ nest 2 $ sep $ f <+> arg : args
instance Pretty lk => PrettyF (ExpLetF lk) where
    prettyF' (ExpLetF' (pretty -> lk) (Vector.toList -> binds) body) =
        parens $
            flatAlt
            (vsep [styleKw lk <+> parens (align (vsep (map prettyBinds binds))), indent 2 body])
            (styleKw lk <+> parens (hsep (map prettyBinds binds)) <+> align body)
        where
            prettyBinds :: (Text, Doc ASTStyle) -> Doc ASTStyle
            prettyBinds (x, e) = brackets $ styleVar (pretty x) <+> align e
instance PrettyF ExpLambdaF where
    prettyF' (ExpLambdaF' (Vector.toList -> args) body) =
        let args' = parens (hsep (map (styleVar . pretty) args))
         in parens $
             flatAlt
             (vsep [ styleKw "lambda" <+> args'
                   , indent 2 $ align body
                   ])
             (styleKw "lambda" <+> args' <+> body)

instance (Pretty name, Forall exp Functor, Forall exp PrettyF)
  => PrettyF (StmtValF name (OpenADT exp)) where
    prettyF' (StmtValF' x e) = parens $
        vsep [ styleKw "val" <+> pretty x, indent 2 $ prettyF e ]
instance (Pretty name, Forall exp Functor, Forall exp PrettyF)
  => PrettyF (StmtDefineF name (OpenADT exp)) where
    prettyF' (StmtDefineF' f (Vector.toList -> args) body) = parens $
        let args' = parens (hsep (map (styleVar . pretty) args))
         in vsep [styleKw "define" <+> styleVar (pretty f) <+> args', indent 2 $ prettyF body]
instance (Forall exp Functor, Forall exp PrettyF) 
  => PrettyF (StmtExpF (OpenADT exp)) where
    prettyF' (StmtExpF' e) = prettyF e
instance (Forall exp Functor, Forall exp PrettyF)
  => PrettyF (StmtCheckExpectF (OpenADT exp)) where
    prettyF' (StmtCheckExpectF' e1 _ e2 _) = parens $
        styleKw "check-expect" <+> prettyF e1 <+> prettyF e2
instance (Forall exp Functor, Forall exp PrettyF)
  => PrettyF (StmtCheckAssertF (OpenADT exp)) where
    prettyF' (StmtCheckAssertF' e _) = parens $
        styleKw "check-assert" <+> prettyF e

