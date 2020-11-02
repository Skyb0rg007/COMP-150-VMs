{-# LANGUAGE TemplateHaskell #-}
{-
   Module:      Uft.Scheme.Ast
   Description: Definitions for the original Scheme datatype
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   These datatypes are parsed by Uft.Scheme.Parse
   See Type.OpenADT to see how these datatypes fit together
-}

module Uft.Scheme.Ast
    ( module Uft.Scheme.Ast
    ) where

import           Data.Char            (isPrint, ord)
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Kind            (Type)
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector
import           Data.Word            (Word8)
import           Type.OpenADT
import           Type.OpenADT.TH      (deriveOpenADT)
import           Uft.Pretty
import           Uft.Util             (derive)

-- * Literals
-- Invariant: Literals only contain Literals

-- | Integer literal: 12 #d123 3.14159 #b010101 #xdeadbeef
newtype LitNumF (a :: Type) = LitNumF' Double
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | String literal: "foo" "bar baz"
newtype LitStrF (a :: Type) = LitStrF' Text
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Symbol literal: foo |bar baz|
newtype LitSymF (a :: Type) = LitSymF' Text
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Char literal: #\x #\space #\x64
newtype LitCharF (a :: Type) = LitCharF' Char
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Boolean literal: #t #f #true #false
newtype LitBoolF (a :: Type) = LitBoolF' Bool
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | List literal: (a b c)
newtype LitListF (a :: Type) = LitListF' (Vector a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Dotted list literal: (a b . c)
data LitDotListF a = LitDotListF' !(Vector a) !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Vector literal: #(a b c)
newtype LitVectorF a = LitVectorF' (Vector a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Bytevector literal: #u8(1 2 3)
newtype LitByteVecF (a :: Type) = LitByteVecF' (Vector Word8)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Unquote a literal
newtype LitUnquoteF a = LitUnquoteF' a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Unquote-splicing
newtype LitUnquoteSplicingF a = LitUnquoteSplicingF' a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- * Expressions
-- Invariant: Expressions contain Expressions or Literals

-- | Quote a literal
newtype ExpQuoteF a = ExpQuoteF' a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Quasi-quote a literal
newtype ExpQuasiQuoteF a = ExpQuasiQuoteF' a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Generic variable reference
newtype ExpVarF (a :: Type) = ExpVarF' Text
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Generic set expression
data ExpSetF a = ExpSetF' !Text !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | If expression
data ExpIfF a = ExpIfF' !a !a !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | While expression
data ExpWhileF a = ExpWhileF' !a !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Begin expression
newtype ExpBeginF a = ExpBeginF' (Vector a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Generic apply expression
data ExpApplyF a = ExpApplyF' !a !(Vector a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Let expression
data ExpLetF a = ExpLetF' !(Vector (Text, a)) !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | LetRec expression
data ExpLetRecF a = ExpLetRecF' !(Vector (Text, a)) !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | LetStar expression
data ExpLetStarF a = ExpLetStarF' !(Vector (Text, a)) !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Lambda expression
data ExpLambdaF a = ExpLambdaF' !(Vector Text) !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- * Statements
-- Invariant: Statements can contain Expressions or Literals

data StmtValF a = StmtValF' !Text !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data StmtDefineF a = StmtDefineF' !Text !(Vector Text) !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data StmtCheckExpectF a = StmtCheckExpectF' !a !Text !a !Text
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data StmtCheckAssertF a = StmtCheckAssertF' !a !Text
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- * Program
-- Invariant: Program cannot contain itself

newtype ProgF a = ProgF' (Vector a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

derive [deriveOpenADT, deriveEq1, deriveOrd1, deriveShow1, deriveRead1]
    [ ''LitNumF, ''LitStrF, ''LitSymF, ''LitCharF, ''LitBoolF
    , ''LitListF, ''LitDotListF, ''LitVectorF, ''LitByteVecF
    , ''LitUnquoteF, ''LitUnquoteSplicingF
    , ''ExpQuoteF, ''ExpQuasiQuoteF, ''ExpVarF, ''ExpSetF
    , ''ExpIfF, ''ExpWhileF
    , ''ExpBeginF, ''ExpApplyF, ''ExpLetF, ''ExpLetRecF
    , ''ExpLetStarF, ''ExpLambdaF
    , ''StmtValF, ''StmtDefineF
    , ''StmtCheckExpectF, ''StmtCheckAssertF
    , ''ProgF
    ]

instance PrettyF LitNumF where
    prettyF' (LitNumF' n) = styleNum n'
        where
            n' :: Doc ann
            n' = pretty n
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
instance PrettyF LitListF where
    prettyF' (LitListF' (Vector.toList -> xs)) = "(" <> hsep xs <> ")"
instance PrettyF LitDotListF where
    prettyF' (LitDotListF' (Vector.toList -> xs) x) = "(" <> hsep xs <+> "." <+> x <> ")"
instance PrettyF LitVectorF where
    prettyF' (LitVectorF' (Vector.toList -> xs)) = "#(" <> hsep xs <> ")"
instance PrettyF LitByteVecF where
    prettyF' (LitByteVecF' v) = "#u8(" <> hsep (map pretty (Vector.toList v)) <> ")"
instance PrettyF LitUnquoteF where
    prettyF' (LitUnquoteF' e) = "," <> e
instance PrettyF LitUnquoteSplicingF where
    prettyF' (LitUnquoteSplicingF' e) = ",@" <> e

instance PrettyF ExpQuoteF where
    prettyF' (ExpQuoteF' x) = squote <> x
instance PrettyF ExpQuasiQuoteF where
    prettyF' (ExpQuasiQuoteF' x) = "`" <> x
instance PrettyF ExpVarF where
    prettyF' (ExpVarF' x) = pretty x
instance PrettyF ExpSetF where
    prettyF' (ExpSetF' x e) = parens $
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
prettyLet :: Doc ASTStyle -> Vector (Text, Doc ASTStyle) -> Doc ASTStyle -> Doc ASTStyle
prettyLet lk (Vector.toList -> binds) body = parens $
    flatAlt
    (vsep [styleKw lk <+> parens (align (vsep (map prettyBinds binds))), indent 2 body])
    (styleKw lk <+> parens (hsep (map prettyBinds binds)) <+> align body)
    where
        prettyBinds :: (Text, Doc ASTStyle) -> Doc ASTStyle
        prettyBinds (x, e) = brackets $ styleVar (pretty x) <+> align e
instance PrettyF ExpLetF where
    prettyF' (ExpLetF' binds body) = prettyLet "let" binds body
instance PrettyF ExpLetStarF where
    prettyF' (ExpLetStarF' binds body) = prettyLet "let*" binds body
instance PrettyF ExpLetRecF where
    prettyF' (ExpLetRecF' binds body) = prettyLet "letrec" binds body
instance PrettyF ExpLambdaF where
    prettyF' (ExpLambdaF' (Vector.toList -> args) body) =
        let args' = parens (hsep (map (styleVar . pretty) args))
         in parens $
             flatAlt
             (vsep [ styleKw "lambda" <+> args'
                   , indent 2 $ align body
                   ])
             (styleKw "lambda" <+> args' <+> body)

instance PrettyF StmtValF where
    prettyF' (StmtValF' x e) = parens $ vsep [ styleKw "val" <+> pretty x, indent 2 e ]
instance PrettyF StmtDefineF where
    prettyF' (StmtDefineF' f (Vector.toList -> args) body) = parens $
        let args' = parens (hsep (map (styleVar . pretty) args))
         in vsep [styleKw "define" <+> styleVar (pretty f) <+> args', indent 2 body]
instance PrettyF StmtCheckExpectF where
    prettyF' (StmtCheckExpectF' e1 _ e2 _) = parens $ styleKw "check-expect" <+> e1 <+> e2
instance PrettyF StmtCheckAssertF where
    prettyF' (StmtCheckAssertF' e _) = parens $ styleKw "check-assert" <+> e

instance PrettyF ProgF where
    prettyF' (ProgF' stmts) = vsep' stmts
