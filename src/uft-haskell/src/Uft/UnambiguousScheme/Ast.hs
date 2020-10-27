{-
   Module:      Uft.UnambiguousScheme.Ast
   Description: AST for the UnambiguousScheme representation
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.UnambiguousScheme.Ast
    ( module Uft.UnambiguousScheme.Ast
    ) where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty (pretty))
import           Type.OpenADT
import           Uft.AstNodes
import           Uft.Primitives

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
data LetKind = LKLet | LKLetRec
    deriving (Show, Eq, Ord)
instance Pretty LetKind where
    pretty LKLet     = "let"
    pretty LKLetRec  = "letrec"

type ExpRowF =
    ( "lit"       .== ExpLitF Lit
   .+ "varLocal"  .== ExpVarLocalF Name
   .+ "varGlobal" .== ExpVarGlobalF
   .+ "setLocal"  .== ExpSetLocalF Name
   .+ "setGlobal" .== ExpSetGlobalF
   .+ "if"        .== ExpIfF
   .+ "while"     .== ExpWhileF
   .+ "begin"     .== ExpBeginF
   .+ "apply"     .== ExpApplyF
   -- .+ "applyPrim" .== ExpApplyPrimF
   .+ "let"       .== ExpLetF LetKind
   .+ "lambda"    .== ExpLambdaF
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
   -- .+ "pair"    .== LitPairF
   -- .+ "vector"  .== LitVectorF
   -- .+ "byteVec" .== LitByteVecF
    )
type LitF = VarF LitRowF
-- | Literals
type Lit = OpenADT LitRowF
