
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}

module Uft.Scheme.Ast
    ( module Uft.Scheme.Ast
    ) where

import           Data.Char                 (isPrint, ord)
import           Data.Constraint           (withDict, type (:-))
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Functor.Foldable     (cata)
import           Data.Row.Internal         (Unconstrained1)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Data.Word                 (Word8)
import           Text.Read                 (Read (readPrec))
import           Type.OpenADT
import           Type.OpenADT.TH
import           Type.VarF
import           Uft.Pretty
import           Data.Kind (Type)
import           Uft.Util

-- * Literals
openADT (drop 3) [d|
    -- | Integer literal: 12 #d123 3.14159 #b010101 #xdeadbeef
    newtype LitNumF a = LitNumF' Double
        deriving Functor
    -- | String literal: "foo" "bar baz"
    newtype LitStrF a = LitStrF' Text
        deriving Functor
    -- | Symbol literal: foo |bar baz|
    newtype LitSymF a = LitSymF' Text
        deriving Functor
    -- | Char literal: #\x #\space #\x64
    newtype LitCharF a = LitCharF' Char
        deriving Functor
    -- | Boolean literal: #t #f #true #false
    newtype LitBoolF a = LitBoolF' Bool
        deriving Functor
    -- | Empty list literal: ()
    data LitEmptyF a = LitEmptyF'
        deriving Functor
    -- | Pair literal: (a . b)
    data LitPairF a = LitPairF' !a !a
        deriving Functor
    -- | Vector literal: #(a b c)
    newtype LitVectorF a = LitVectorF' (Vector a)
        deriving Functor
    -- | Bytevector literal: #u8(1 2 3)
    newtype LitByteVecF a = LitByteVecF' (Vector Word8)
        deriving Functor
    |]
foldMapM openADTDerive1 [''LitNumF, ''LitStrF, ''LitSymF, ''LitCharF, ''LitBoolF, ''LitEmptyF, ''LitPairF, ''LitVectorF, ''LitByteVecF]

type LitRowF =
    ( "num"     .== LitNumF
   .+ "str"     .== LitStrF
   .+ "sym"     .== LitSymF
   .+ "char"    .== LitCharF
   .+ "bool"    .== LitBoolF
   .+ "empty"   .== LitEmptyF
   .+ "pair"    .== LitPairF
   .+ "vector"  .== LitVectorF
   .+ "byteVec" .== LitByteVecF
    )
type LitF = VarF LitRowF
type Lit = OpenADT LitRowF

-- * Expressions

data LetKind = Let | LetRec | LetStar
    deriving (Show, Eq, Ord)

openADT (drop 3) [d|
    newtype ExpLitF a = ExpLitF' Lit
        deriving Functor
    newtype ExpVarF a = ExpVarF' Text
        deriving Functor
    data ExpSetF a = ExpSetF' !Text !a
        deriving Functor
    data ExpIfF a = ExpIfF' !a !a !(Maybe a)
        deriving Functor
    data ExpWhileF a = ExpWhileF' !a !(Vector a)
        deriving Functor
    newtype ExpBeginF a = ExpBeginF' (Vector a)
        deriving Functor
    data ExpApplyF a = ExpApplyF' !a !(Vector a)
        deriving Functor
    data ExpLetF a = ExpLetF' !LetKind !(Vector (Text, a)) !(Vector a)
        deriving Functor
    data ExpLambdaF a = ExpLambdaF' !(Vector Text) !(Vector a)
        deriving Functor
    |]
foldMapM openADTDerive1 [''ExpLitF, ''ExpVarF, ''ExpSetF, ''ExpIfF, ''ExpWhileF, ''ExpBeginF, ''ExpApplyF, ''ExpLetF, ''ExpLambdaF]

type ExpRowF =
    ( "lit"     .== ExpLitF
   .+ "var"     .== ExpVarF
   .+ "set"     .== ExpSetF
   .+ "if"      .== ExpIfF
   .+ "while"   .== ExpWhileF
   .+ "begin"   .== ExpBeginF
   .+ "apply"   .== ExpApplyF
   .+ "let"     .== ExpLetF
   .+ "lambda"  .== ExpLambdaF
    )
type ExpF = VarF ExpRowF
type Exp = OpenADT ExpRowF

-- * Statements

openADT (drop 4) [d|
    data StmtValF a = StmtValF' !Text !Exp
        deriving Functor
    data StmtDefineF a = StmtDefineF' !Text !(Vector Text) !Exp
        deriving Functor
    newtype StmtExpF a = StmtExpF' Exp
        deriving Functor
    data StmtCheckExpectF a = StmtCheckExpectF' !Exp !Exp
        deriving Functor
    newtype StmtCheckAssertF a = StmtCheckAssertF' Exp
        deriving Functor
    |]
foldMapM openADTDerive1 [''StmtValF, ''StmtDefineF, ''StmtExpF, ''StmtCheckExpectF, ''StmtCheckAssertF]

type StmtRowF =
    ( "val"         .== StmtValF
   .+ "define"      .== StmtDefineF
   .+ "exp"         .== StmtExpF
   .+ "checkExpect" .== StmtCheckExpectF
   .+ "checkAssert" .== StmtCheckAssertF
    )
type StmtF = VarF StmtRowF
type Stmt = OpenADT StmtRowF

-- * A program is a list of statements
type Prog = [Stmt]

-- * Pretty-printing

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

requiresQuoting
    :: forall r x q.
        ( q ~ ("str" .== LitStrF .+ "char" .== LitCharF .+ "num" .== LitNumF)
        , Forall (ApRow r x .\\ ApRow q x) Unconstrained1
        , (ApRow r x .\\ ApRow q x) ~ ApRow (r .\\ q) x
        )
    => VarF r x
    -> Bool
requiresQuoting adt =
    case multiTrialF @q adt of
      Left _  -> False
      Right _ -> True

-- | Print a literal, collapsing pairs into list syntax
prettyLitList :: Lit -> Doc ASTStyle
prettyLitList LitEmpty = "()"
prettyLitList (LitPair a b) = go b (prettyF a :)
    where
        go :: Lit -> ([Doc ASTStyle] -> [Doc ASTStyle]) -> Doc ASTStyle
        go LitEmpty f      = parens $ hsep $ f []
        go (LitPair a b) f = go b (f . (prettyF a :))
        go lit f           = parens $ hsep $ f [] ++ [".", prettyF lit]
prettyLitList lit = prettyF lit

instance Pretty LetKind where
    pretty Let     = "let"
    pretty LetRec  = "letrec"
    pretty LetStar = "let*"

instance PrettyF ExpLitF where
    prettyF' (ExpLitF' lit)
      | requiresQuoting (unfix lit) = squote <> prettyLitList lit
      | otherwise = prettyLitList lit
instance PrettyF ExpVarF where
    prettyF' (ExpVarF' x) = pretty x
instance PrettyF ExpSetF where
    prettyF' (ExpSetF' x e) = parens $
        styleKw "set" <+> styleVar (pretty x) <+> align e
instance PrettyF ExpIfF where
    prettyF' (ExpIfF' cond t (Just f)) = parens $
        flatAlt
        (vsep [styleKw "if" <+> cond, indent 2 $ vsep [t, f]])
        (styleKw "if" <+> cond <+> t <+> f)
    prettyF' (ExpIfF' cond t Nothing) = parens $
        flatAlt
        (vsep [styleKw "if" <+> cond, indent 2 t])
        (styleKw "if" <+> cond <+> t)
instance PrettyF ExpWhileF where
    prettyF' (ExpWhileF' cond (Vector.toList -> body)) = parens $
        flatAlt
        (vsep [styleKw "while" <+> cond, indent 2 $ vsep body])
        (styleKw "while" <+> cond <+> hsep body)
instance PrettyF ExpBeginF where
    prettyF' (ExpBeginF' (Vector.toList -> es)) = parens $
        flatAlt
        (vsep [styleKw "begin", indent 2 $ vsep es])
        (hsep $ styleKw "begin" : es)
instance PrettyF ExpApplyF where
    prettyF' (ExpApplyF' f (Vector.toList -> [])) = parens f
    prettyF' (ExpApplyF' f (Vector.toList -> (arg:args))) = parens $
        align $ nest 2 $ sep $ f <+> arg : args
instance PrettyF ExpLetF where
    prettyF' (ExpLetF' (pretty ->lk) (Vector.toList -> binds) (Vector.toList -> body)) =
        parens $
            flatAlt
            (vsep [styleKw lk <+> parens (align (vsep (map prettyBinds binds))), indent 2 $ vsep body])
            (styleKw lk <+> parens (hsep (map prettyBinds binds)) <+> hsep body)
        where
            prettyBinds :: (Text, Doc ASTStyle) -> Doc ASTStyle
            prettyBinds (x, e) = brackets $ styleVar (pretty x) <+> align e
instance PrettyF ExpLambdaF where
    prettyF' (ExpLambdaF' (Vector.toList -> args) (Vector.toList -> body)) =
        let args' = parens (hsep (map (styleVar . pretty) args))
         in parens $
             flatAlt
             (vsep [ styleKw "lambda" <+> args'
                   , indent 2 $ align $ vsep body
                   ])
             (styleKw "lambda" <+> args' <+> hsep body)

instance PrettyF StmtValF where
    prettyF' (StmtValF' x e) = parens $
        vsep [ styleKw "val" <+> pretty x, indent 2 $ prettyF e ]
instance PrettyF StmtDefineF where
    prettyF' (StmtDefineF' f (Vector.toList -> args) body) = parens $
        let args' = parens (hsep (map (styleVar . pretty) args))
         in vsep [styleKw "define" <+> styleVar (pretty f) <+> args', indent 2 $ prettyF body]
instance PrettyF StmtExpF where
    prettyF' (StmtExpF' e) = prettyF e
instance PrettyF StmtCheckExpectF where
    prettyF' (StmtCheckExpectF' e1 e2) = parens $
        styleKw "check-expect" <+> prettyF e1 <+> prettyF e2
instance PrettyF StmtCheckAssertF where
    prettyF' (StmtCheckAssertF' e) = parens $
        styleKw "check-assert" <+> prettyF e

