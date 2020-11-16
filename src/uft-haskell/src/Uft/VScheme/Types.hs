{-# LANGUAGE StrictData, TemplateHaskell #-}
{-
   Module:      Uft.VScheme.Types
   Description: Type definitions for the VScheme language
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This file defines the variants that appear in the VScheme source language
-}

module Uft.VScheme.Types
    ( module Uft.VScheme.Types
    ) where

import           Data.Deriving
import           Data.Kind       (Type)
import           Data.Maybe      (catMaybes)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Word       (Word8)
import           Debug.Trace
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Parse
import           Uft.Pretty
import           Uft.SExpr.Types
import           Uft.Util
import           Uft.Primitives

-- | Variants that are part of the VScheme language datatype
-- Note that the order matters for parsing
type VSchemeR =
    -- Quoting
    '[ EQuoteF
     , EQuasiF
     , EUnquoteF
     , EUnquoteSplicingF
    -- Self-evaluating literals
     , LBoolF
     , LByteVecF
     , LCharF
     , LNumF
     , LStrF
    -- Literals that require quoting
     , LEmptyF
     , LPairF
     , LSymF
     , LVectorF
    -- Definitions
     , DValF
     , DDefineF
     , DCheckExpectF
     , DCheckAssertF
    -- Derived expressions
     , ELetStarF
     , ELetRecF
    -- Primitive expressions
     , EBeginF
     , ELambdaF
     , ESetF
     , EIfF
     , ELetF
     , EWhileF
     , EApplyF -- Must be last
     , EVarF
     , EPrimApplyF -- Note that this cannot be parsed
     ]

-- * Atoms

-- | Numeric literal
newtype LNumF (a :: Type) = LNumF' Double
    deriving (Functor, Foldable, Traversable)

instance PrettyF LNumF where
    prettyF' (LNumF' n) = pretty (showSNum n "")

-- | Numbers are always parsed from number atoms
instance ParseF LNumF where
    parseF' _ (SAtom (SNum n)) = pure $ LNumF' n
    parseF' _ _ = empty

-- | Symbol literal
newtype LSymF (a :: Type) = LSymF' Text
    deriving (Functor, Foldable, Traversable)

instance PrettyF LSymF where
    prettyF' (LSymF' s) = pretty (showSSymbol (Text.unpack s) "")

-- | Symbols are parsed from symbol atoms when in quote context
instance ParseF LSymF where
    parseF' PCQ{} (SAtom (SSymbol s)) = pure $ LSymF' s
    parseF' _ _ = empty

-- | String literal
newtype LStrF (a :: Type) = LStrF' Text
    deriving (Functor, Foldable, Traversable)

instance PrettyF LStrF where
    prettyF' (LStrF' x) = pretty (showSString (Text.unpack x) "")

-- | Strings are always parsed from string atoms
instance ParseF LStrF where
    parseF' _ (SAtom (SString s)) = pure $ LStrF' s
    parseF' _ _ = empty

-- | Character literal
newtype LCharF (a :: Type) = LCharF' Char
    deriving (Functor, Foldable, Traversable)

instance PrettyF LCharF where
    prettyF' (LCharF' c) = pretty (showSChar c "")

-- | Characters are always parsed from character atoms
instance ParseF LCharF where
    parseF' _ (SAtom (SChar c)) = pure $ LCharF' c
    parseF' _ _ = empty

-- | Boolean literal
newtype LBoolF (a :: Type) = LBoolF' Bool
    deriving (Functor, Foldable, Traversable)

instance PrettyF LBoolF where
    prettyF' (LBoolF' b) = pretty (showSBool b "")

-- | Booleans are always parsed from boolean atoms
instance ParseF LBoolF where
    parseF' _ (SAtom (SBool b)) = pure $ LBoolF' b
    parseF' _ _ = empty

-- | ByteVector literal
newtype LByteVecF (a :: Type) = LByteVecF' [Word8]
    deriving (Functor, Foldable, Traversable)

instance PrettyF LByteVecF where
    prettyF' (LByteVecF' bv) = pretty (showSByteVector bv "")

-- | Bytevectors are always parsed bytevector atoms
instance ParseF LByteVecF where
    parseF' _ (SAtom (SByteVector bv)) = pure $ LByteVecF' bv
    parseF' _ _ = empty

-- | Vector literal
data LVectorF a = LVectorF' [a]
    deriving (Functor, Foldable, Traversable)

instance PrettyF LVectorF where
    prettyF' (LVectorF' v) = pretty (showSVector v "")

-- | Vectors are parsed in quoted context
-- Note that this implementation currently errors out if the vector is not quoted
instance ParseF LVectorF where
    parseF' (PCQuasi n _) (SVector xs) = pure $ LVectorF' (map (PCQuasi n True,) xs)
    parseF' PCQuote (SVector xs) = pure $ LVectorF' (map (PCQuote,) xs)
    parseF' _ (SVector _) = fail "Vector found in non-quoted position"
    parseF' _ _ = empty

-- | List literal
data LPairF a = LPairF' a a
    deriving (Functor, Foldable, Traversable)

instance PrettyF LPairF where
    prettyF' (LPairF' a b) = "(" <> a <+> "." <+> b <> ")"

-- | Lists are parsed in quoted context
-- Note that this implementation ignores unquote and unquote-splicing
instance ParseF LPairF where
    parseF' (PCQuasi n _) (SCons a b) =
         pure $ LPairF' (PCQuasi n True, a) (PCQuasi n False, b)
    parseF' PCQuote (SCons a b) = pure $ LPairF' (PCQuote, a) (PCQuote, b)
    parseF' _ _ = empty

-- | Empty list
data LEmptyF (a :: Type) = LEmptyF'
    deriving (Functor, Foldable, Traversable)

instance PrettyF LEmptyF where
    prettyF' LEmptyF' = "()"

-- | Empty list is parsed in quoted context
instance ParseF LEmptyF where
    parseF' PCQ{} SNil = pure LEmptyF'
    parseF' PCNotQ (SList ["quote", SNil]) = pure LEmptyF'
    parseF' _ _ = empty

-- | Begin expression
data EBeginF a = EBeginF' [a]
    deriving (Functor, Foldable, Traversable)

instance PrettyF EBeginF where
    prettyF' (EBeginF' es) = parens $
        flatAlt
        (vsep [styleKw "begin", indent 2 $ vsep es])
        (hsep $ styleKw "begin" : es)

-- | Begin is parsed if not quoted
instance ParseF EBeginF where
    parseF' PCNotQ (SList ("begin" : es)) = pure $ EBeginF' (map (PCExp,) es)
    parseF' _ _ = empty

-- * Let

-- | let
data ELetF a = ELetF' [(Text, a)] a
    deriving (Functor, Foldable, Traversable)

-- | let*
data ELetStarF a = ELetStarF' [(Text, a)] a
    deriving (Functor, Foldable, Traversable)

-- | letrec
data ELetRecF a = ELetRecF' [(Text, a)] a
    deriving (Functor, Foldable, Traversable)

-- | letrec*
data ELetRecStarF a = ELetRecStarF' [(Text, a)] a
    deriving (Functor, Foldable, Traversable)

prettyLet :: Doc ASTStyle -> [(Text, Doc ASTStyle)] -> Doc ASTStyle -> Doc ASTStyle
prettyLet lk binds body = parens $
    flatAlt
    (vsep [styleKw lk <+> parens (align (vsep (map prettyBinds binds))), indent 2 body])
    (styleKw lk <+> parens (hsep (map prettyBinds binds)) <+> align body)
    where
        prettyBinds :: (Text, Doc ASTStyle) -> Doc ASTStyle
        prettyBinds (x, e) = brackets $ styleVar (pretty x) <+> align e

instance PrettyF ELetF where
    prettyF' (ELetF' binds body) = prettyLet "let" binds body
instance PrettyF ELetStarF where
    prettyF' (ELetStarF' binds body) = prettyLet "let*" binds body
instance PrettyF ELetRecF where
    prettyF' (ELetRecF' binds body) = prettyLet "letrec" binds body
instance PrettyF ELetRecStarF where
    prettyF' (ELetRecStarF' binds body) = prettyLet "letrec*" binds body

matchBinds :: SExpr -> Maybe [(Text, (ParseContext, SExpr))]
matchBinds (SList binds) = traverse matchBind binds
matchBinds _ = Nothing

matchBind :: SExpr -> Maybe (Text, (ParseContext, SExpr))
matchBind (SList [SAtom (SSymbol x), e]) = Just (x, (PCExp, e))
matchBind _ = Nothing

-- | Let expressions are parsed when not quoted
instance ParseF ELetF where
    parseF' PCNotQ (SList ("let" : (matchBinds -> Just binds) : body)) =
        pure $ ELetF' binds (PCExp, SList ("begin" : body))
    parseF' PCNotQ (SCons "let" _) = fail "Invalid let syntax"
    parseF' _ _ = empty

-- | Let* expressions are parsed when not quoted
instance ParseF ELetStarF where
    parseF' PCNotQ (SList ("let*" : (matchBinds -> Just binds) : body)) =
        pure $ ELetStarF' binds (PCExp, SList ("begin" : body))
    parseF' PCNotQ (SCons "let*" _) = fail "Invalid let* syntax"
    parseF' _ _ = empty

-- | Letrec expressions are parsed when not quoted
instance ParseF ELetRecF where
    parseF' PCNotQ (SList ("letrec" : (matchBinds -> Just binds) : body)) =
        pure $ ELetRecF' binds (PCExp, SList ("begin" : body))
    parseF' PCNotQ (SCons "letrec" _) = fail "Invalid letrec syntax"
    parseF' _ _ = empty

-- | Letrec* expressions are parsed when not quoted
instance ParseF ELetRecStarF where
    parseF' PCNotQ (SList ("letrec*" : (matchBinds -> Just binds) : body)) =
        pure $ ELetRecStarF' binds (PCExp, SList ("begin" : body))
    parseF' PCNotQ (SCons "letrec*" _) = fail "Invalid letrec* syntax"
    parseF' _ _ = empty

-- * Quoting

-- | (quote e)
newtype EQuoteF a = EQuoteF' a
    deriving (Functor, Foldable, Traversable)

instance PrettyF EQuoteF where
    prettyF' (EQuoteF' x) = "'" <> x

instance ParseF EQuoteF where
    parseF' PCNotQ (SList ["quote", x]) = pure $ EQuoteF' (PCQuote, x)
    parseF' PCNotQ (SCons "quote" _) = fail "Invalid quote syntax"
    parseF' _ _ = empty

-- | (quasiquote e)
newtype EQuasiF a = EQuasiF' a
    deriving (Functor, Foldable, Traversable)

instance PrettyF EQuasiF where
    prettyF' (EQuasiF' x) = "`" <> x

instance ParseF EQuasiF where
    parseF' PCNotQ (SList ["quasiquote", x]) =
        pure $ EQuasiF' (PCQuasi 1 False, x)
    parseF' PCNotQ (SCons "quasiquote" _) = fail "Invalid quasiquote syntax"
    parseF' (PCQuasi n _) (SList ["quasiquote", x]) =
        pure $ EQuasiF' (PCQuasi (succ n) False, x)
    parseF' _ _ = empty

-- | (unquote e)
newtype EUnquoteF a = EUnquoteF' a
    deriving (Functor, Foldable, Traversable)

instance PrettyF EUnquoteF where
    prettyF' (EUnquoteF' x) = "," <> x

instance ParseF EUnquoteF where
    parseF' PCNotQ (SCons "unquote" _) = fail "Invalid unquote syntax"
    parseF' (PCQuasi 1 _) (SList ["unquote", x]) =
        pure $ EUnquoteF' (PCExp, x)
    parseF' (PCQuasi n _) (SList ["unquote", x]) =
        pure $ EUnquoteF' (PCQuasi (pred n) False, x)
    parseF' _ _ = empty

newtype EUnquoteSplicingF a = EUnquoteSplicingF' a
    deriving (Functor, Foldable, Traversable)

instance PrettyF EUnquoteSplicingF where
    prettyF' (EUnquoteSplicingF' x) = ",@" <> x

instance ParseF EUnquoteSplicingF where
    parseF' PCNotQ (SCons "unquote-splicing" _) = fail "Invalid unquote-splicing syntax"
    parseF' (PCQuasi 1 True) (SList ["unquote-splicing", x]) =
        pure $ EUnquoteSplicingF' (PCExp, x)
    parseF' (PCQuasi n True) (SList ["unquote-splicing", x]) =
        pure $ EUnquoteSplicingF' (PCQuasi (pred n) False, x)
    parseF' _ _ = empty

-- | set!
data ESetF a = ESetF' Text a
    deriving (Functor, Foldable, Traversable)

instance PrettyF ESetF where
    prettyF' (ESetF' x e) = parens $
        styleKw "set!" <+> styleVar (pretty x) <+> align e

instance ParseF ESetF where
    parseF' PCNotQ (SList ["set!", SAtom (SSymbol x), e]) =
        pure $ ESetF' x (PCExp, e)
    parseF' PCNotQ (SCons "set!" _) = fail "Invalid set! syntax"
    parseF' _ _ = empty

-- | if
data EIfF a = EIfF' a a a
    deriving (Functor, Foldable, Traversable)

instance PrettyF EIfF where
    prettyF' (EIfF' cond t f) = parens $
        flatAlt
        (vsep [styleKw "if" <+> cond, indent 2 $ vsep [t, f]])
        (styleKw "if" <+> cond <+> t <+> f)

instance ParseF EIfF where
    parseF' PCNotQ (SList ["if", cond, t, f]) =
        pure $ EIfF' (PCExp, cond) (PCExp, t) (PCExp, f)
    parseF' PCNotQ (SCons "if" _) =
        fail "Invalid if syntax"
    parseF' _ _ = empty

-- | while
data EWhileF a = EWhileF' a a
    deriving (Functor, Foldable, Traversable)

instance PrettyF EWhileF where
    prettyF' (EWhileF' cond body) = parens $
        flatAlt
        (vsep [styleKw "while" <+> cond, indent 2 body])
        (styleKw "while" <+> cond <+> body)

instance ParseF EWhileF where
    parseF' PCNotQ (SList ("while" : cond : body)) =
        pure $ EWhileF' (PCExp, cond) (PCExp, SList ("begin" : body))
    parseF' PCNotQ (SCons "while" _) =
        fail "Invalid while syntax"
    parseF' _ _ = empty

data ELambdaF a = ELambdaF' [Text] a
    deriving (Functor, Foldable, Traversable)

instance PrettyF ELambdaF where
    prettyF' (ELambdaF' args body) = parens $
        "lambda" <+> parens (hsep (map pretty args))
        <+> body

isSym (SAtom (SSymbol s)) = Just s
isSym _ = Nothing

instance ParseF ELambdaF where
    parseF' PCNotQ (SList ("lambda" : SList (traverse isSym -> Just args) : body)) =
        pure $ ELambdaF' args (PCExp, SList ("begin" : body))
    parseF' PCNotQ (SCons "lambda" _) = fail "Invalid lambda syntax"
    parseF' _ _ = empty

-- | Primitives
data EPrimApplyF a = EPrimApplyF' Prim [a]
    deriving (Functor, Foldable, Traversable)

instance PrettyF EPrimApplyF where
    prettyF' (EPrimApplyF' p args) = parens $
        hsep ("%primcall" : stylePrim (pretty (_prim_name p)) : args)

-- | Primitive application cannot be parsed
instance ParseF EPrimApplyF where
    parseF' _ _ = empty

-- | Variables
data EVarF (a :: Type) = EVarF' Text
    deriving (Functor, Foldable, Traversable)

instance PrettyF EVarF where
    prettyF' (EVarF' x) = styleVar (pretty x)

instance ParseF EVarF where
    parseF' PCNotQ (SAtom (SSymbol x)) = pure $ EVarF' x
    parseF' _ _ = empty

-- | Application
data EApplyF a = EApplyF' a [a]
    deriving (Functor, Foldable, Traversable)

instance PrettyF EApplyF where
    prettyF' (EApplyF' f args) = parens $
        case args of
          [] -> f
          x:xs -> align $ nest 2 $ sep $ f <+> x : xs

-- NOTE: must be last in the type-list
instance ParseF EApplyF where
    parseF' PCNotQ (SList (f : args)) =
        pure $ EApplyF' (PCExp, f) (map (PCExp,) args)
    parseF' _ _ = empty

-- * Definitions

data DCheckAssertF a = DCheckAssertF' Text a
    deriving (Functor, Foldable, Traversable)

instance PrettyF DCheckAssertF where
    prettyF' (DCheckAssertF' _ x) = "(check-assert" <+> x <> ")"

instance ParseF DCheckAssertF where
    parseF' PCTop (SList ["check-assert", x]) =
        pure $ DCheckAssertF' (Text.pack (show x)) (PCExp, x)
    parseF' PCTop (SCons "check-assert" _) = fail "Invalid check-assert syntax"
    parseF' _ _ = empty

data DCheckExpectF a = DCheckExpectF' Text a Text a
    deriving (Functor, Foldable, Traversable)

instance PrettyF DCheckExpectF where
    prettyF' (DCheckExpectF' _ x _ y) = "(check-expect" <+> x <+> y <> ")"

instance ParseF DCheckExpectF where
    parseF' PCTop (SList ["check-expect", x, y]) =
        pure $ DCheckExpectF' (Text.pack (show x)) (PCExp, x) (Text.pack (show y)) (PCExp, y)
    parseF' PCTop (SCons "check-expect" _) = fail "Invalid check-expect syntax"
    parseF' _ _ = empty

data DValF a = DValF' Text a
    deriving (Functor, Foldable, Traversable)

instance PrettyF DValF where
    prettyF' (DValF' x e) = parens $ vsep [ styleKw "val" <+> pretty x, indent 2 e ]

instance ParseF DValF where
    parseF' PCTop (SList ["val", SAtom (SSymbol x), e]) =
        pure $ DValF' x (PCExp, e)
    parseF' PCTop (SCons "val" _) = fail "Invalid val syntax"
    parseF' _ _ = empty

data DDefineF a = DDefineF' Text [Text] a
    deriving (Functor, Foldable, Traversable)

instance PrettyF DDefineF where
    prettyF' (DDefineF' f args body) = parens $
        let args' = parens (hsep (map (styleVar . pretty) args))
         in vsep [styleKw "define" <+> styleVar (pretty f) <+> args', indent 2 body]

instance ParseF DDefineF where
    parseF' PCTop (SList ("define" : SAtom (SSymbol f) : SList args : body))
      | Just args' <- traverse isSymbol args =
        pure $ DDefineF' f args' (PCExp, SList ("begin" : body))
        where
            isSymbol (SAtom (SSymbol x)) = Just x
            isSymbol _ = Nothing
    parseF' PCTop (SCons "define" _) = fail "Invalid define syntax"
    parseF' _ _ = empty

--

derive [deriveOpenADT, deriveShow1, deriveEq1, deriveOrd1, deriveRead1]
    [ ''EQuoteF
    , ''EQuasiF
    , ''EUnquoteF
    , ''EUnquoteSplicingF
    , ''LBoolF
    , ''LByteVecF
    , ''LCharF
    , ''LNumF
    , ''LStrF
    , ''LEmptyF
    , ''LPairF
    , ''LSymF
    , ''LVectorF
    , ''DValF
    , ''DDefineF
    , ''DCheckExpectF
    , ''DCheckAssertF
    , ''ELetStarF
    , ''ELetRecF
    , ''ELambdaF
    , ''EBeginF
    , ''ESetF
    , ''EIfF
    , ''ELetF
    , ''EWhileF
    , ''EApplyF
    , ''EVarF
    , ''EPrimApplyF
    ]


