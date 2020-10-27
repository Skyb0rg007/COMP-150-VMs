{-
   Module:      Uft.Scheme.AST
   Description: AST for the Scheme source code
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.Scheme.Ast
    ( module Uft.Scheme.Ast
    ) where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty (pretty))
import           Type.OpenADT
import           Uft.AstNodes

-- | Names are just 'Text'
type Name = Text

type Prog = [Stmt]

type StmtRowF =
    ( "val"         .== StmtValF Name Exp
   .+ "define"      .== StmtDefineF Name Exp
   .+ "exp"         .== StmtExpF Exp
   .+ "checkExpect" .== StmtCheckExpectF Exp
   .+ "checkAssert" .== StmtCheckAssertF Exp
    )
type StmtF = VarF StmtRowF
-- | Statements
type Stmt = OpenADT StmtRowF

-- | Types of let expressions
data LetKind = LKLet | LKLetRec | LKLetStar
    deriving (Show, Eq, Ord)
instance Pretty LetKind where
    pretty LKLet     = "let"
    pretty LKLetRec  = "letrec"
    pretty LKLetStar = "let*"

type ExpRowF =
    ( "lit"    .== ExpLitF Lit
   .+ "var"    .== ExpVarF Name
   .+ "set"    .== ExpSetF Name
   .+ "if"     .== ExpIfF
   .+ "while"  .== ExpWhileF
   .+ "begin"  .== ExpBeginF
   .+ "apply"  .== ExpApplyF
   .+ "let"    .== ExpLetF LetKind
   .+ "lambda" .== ExpLambdaF
    )
type ExpF = VarF ExpRowF
-- | Expressions
type Exp = OpenADT ExpRowF

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
-- | Literals
type Lit = OpenADT LitRowF

