
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}

module Uft.Scheme.Ast
    ( module Uft.Scheme.Ast
    ) where

import           Data.Deriving
import           Data.Functor.Classes
import           Data.Functor.Foldable     (cata)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Data.Word                 (Word8)
import           Text.Read                 (Read (readPrec))
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Util

-- * Literals

openADT (drop 3) [d|
    -- | Integer literal
    newtype LitNumF a = LitNumF' Double
    -- | String literal
    newtype LitStrF a = LitStrF' Text
    -- | Symbol literal
    newtype LitSymF a = LitSymF' Text
    -- | Char literal
    newtype LitCharF a = LitCharF' Char
    -- | Boolean literal
    newtype LitBoolF a = LitBoolF' Bool
    -- | Empty list literal
    data LitEmptyF a = LitEmptyF'
    -- | Pair literal
    data LitPairF a = LitPairF' !a !a
    -- | Vector literal
    newtype LitVectorF a = LitVectorF' (Vector a)
    -- | Bytevector literal
    newtype LitByteVecF a = LitByteVecF' (Vector Word8)
    |]
foldMapM openADTDerive1 [''LitNumF, ''LitStrF, ''LitSymF, ''LitCharF, ''LitBoolF, ''LitEmptyF, ''LitPairF, ''LitVectorF, ''LitByteVecF]

instance PrettyF LitNumF where
    prettyF' (LitNumF' n) = styleNum (pretty n)
instance PrettyF LitStrF where
    prettyF' (LitStrF' s) = styleString (dquotes (pretty s))
instance PrettyF LitSymF where
    prettyF' (LitSymF' s) = styleSym (pretty s)
instance PrettyF LitCharF where
    prettyF' (LitCharF' c) = styleChar ("#\\" <> pretty c)
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

prettyLitList
    :: forall r.
       ( Forall (r .- "litPair") PrettyF
       , Forall (r .- "litPair") Functor
       , Forall r Functor )
    => OpenADT r
    -> Doc ASTStyle
prettyLitList w = cata alg w False
    where
        -- The boolean is true if the literal is the tail of a pair
        alg :: VarF r (Bool -> Doc ASTStyle) -> Bool -> Doc ASTStyle
        alg w b =
            case trialF w #litPair of
              Left lit -> prettyF' $ fmap ($ False) lit
              Right _ -> ""

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
    newtype ExpVarF a = ExpVarF' Text
    data ExpSetF a = ExpSetF' !Text !a
    data ExpIfF a = ExpIfF' !a !a !(Maybe a)
    data ExpWhileF a = ExpWhileF' !a !(Vector a)
    newtype ExpBeginF a = ExpBeginF' (Vector a)
    data ExpApplyF a = ExpApplyF' !a !(Vector a)
    data ExpLetF a = ExpLetF' !LetKind !(Vector (Text, a)) !(Vector a)
    data ExpLambdaF a = ExpLambdaF' !(Vector Text) !(Vector a)
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

instance Pretty LetKind where
    pretty Let     = "let"
    pretty LetRec  = "letrec"
    pretty LetStar = "let*"

instance PrettyF ExpLitF where
    prettyF' (ExpLitF' lit) = squote <> prettyF lit
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

openADT (drop 4) [d|
    data StmtValF a = StmtValF' !Text !Exp
    data StmtDefineF a = StmtDefineF' !Text !(Vector Text) !Exp
    newtype StmtExpF a = StmtExpF' Exp
    data StmtCheckExpectF a = StmtCheckExpectF' !Exp !Exp
    newtype StmtCheckAssertF a = StmtCheckAssertF' Exp
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

type Prog = [Stmt]

