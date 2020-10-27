
{-# LANGUAGE TemplateHaskell #-}

module Uft.UnambiguousScheme.Ast
    ( module Uft.UnambiguousScheme.Ast
    , module Uft.Scheme.Ast
    ) where

import           Data.Maybe      (isJust)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Primitives
-- import           Uft.Scheme.Ast  hiding (Lit, LitF, LitRowF, LetKind, ExpLetF, ExpLet, ExpLitF)
import           Uft.Scheme.Ast (ExpBeginF, ExpIfF, ExpLambdaF, ExpWhileF,
                                 LitByteVecF, LitCharF, LitEmptyF, LitNumF, LitBoolF,
                                 LitStrF, LitSymF, LitVectorF)
import           Uft.Util
import           Data.Bifunctor.Flip (Flip (..))

type LitRowF =
    ( "num"     .== LitNumF
   .+ "str"     .== LitStrF
   .+ "sym"     .== LitSymF
   .+ "char"    .== LitCharF
   .+ "bool"    .== LitBoolF
   .+ "empty"   .== LitEmptyF
   -- .+ "pair"    .== LitPairF
   .+ "vector"  .== LitVectorF
   .+ "byteVec" .== LitByteVecF
    )
type LitF = VarF LitRowF
type Lit = OpenADT LitRowF

openADT (drop 3) [d|
    data ExpSetLocalF a = ExpSetLocalF' !Text !a
        deriving Functor
    data ExpSetGlobalF a = ExpSetGlobalF' !Text !a
        deriving Functor
    data ExpApplyFunF a = ExpApplyFunF' !a !(Vector a)
        deriving Functor
    data ExpApplyPrimF a = ExpApplyPrimF' !(Flip SomePrim () a)
        deriving Functor
    |]

data LetKind = Let | LetRec -- | LetStar
    deriving (Show, Eq, Ord)

openADT (drop 3) [d|
    newtype ExpLitF a = ExpLitF' Lit
        deriving Functor
    data ExpLetF a = ExpLetF' !LetKind !(Vector (Text, a)) !(Vector a)
        deriving Functor
    data ExpVarLocalF a = ExpVarLocalF' !Text
        deriving Functor
    data ExpVarGlobalF a = ExpVarGlobalF' !Text
        deriving Functor
    |]
-- foldMapM openADTDerive1 [''ExpLitF, ''ExpVarF, ''ExpSetF, ''ExpIfF, ''ExpWhileF, ''ExpBeginF, ''ExpApplyF, ''ExpLetF, ''ExpLambdaF]

type ExpRowF =
    ( "lit"       .== ExpLitF
   .+ "varLocal"  .== ExpVarLocalF
   .+ "varGlobal" .== ExpVarGlobalF
   .+ "setLocal"  .== ExpSetLocalF
   .+ "setGlobal" .== ExpSetGlobalF
   .+ "if"        .== ExpIfF
   .+ "while"     .== ExpWhileF
   .+ "begin"     .== ExpBeginF
   .+ "applyFun"  .== ExpApplyFunF
   .+ "applyPrim" .== ExpApplyPrimF
   .+ "let"       .== ExpLetF
   .+ "lambda"    .== ExpLambdaF
    )
type ExpF = VarF ExpRowF
type Exp = OpenADT ExpRowF


