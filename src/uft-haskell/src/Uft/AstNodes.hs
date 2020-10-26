{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-
   Module:      Uft.AstNodes
   Description: Compositional datatypes for ASTs
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.AstNodes
    ( module Uft.AstNodes
    ) where

import           Data.Bifunctor       (first)
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Kind            (Type)
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import           Data.Word            (Word8)
import           Type.OpenADT.TH      (deriveOpenADT)
import           Uft.Primitives       (SomePrim)
import           Uft.Util             (derive)

-- * Literals
-- | Integer literal: 12 #d123 3.14159 #b010101 #xdeadbeef
newtype LitNumF (a :: Type) = LitNumF' Double
    deriving (Show, Eq, Ord, Functor)
-- | String literal: "foo" "bar baz"
newtype LitStrF (a :: Type) = LitStrF' Text
    deriving (Show, Eq, Ord, Functor)
-- | Symbol literal: foo |bar baz|
newtype LitSymF (a :: Type) = LitSymF' Text
    deriving (Show, Eq, Ord, Functor)
-- | Char literal: #\x #\space #\x64
newtype LitCharF (a :: Type) = LitCharF' Char
    deriving (Show, Eq, Ord, Functor)
-- | Boolean literal: #t #f #true #false
newtype LitBoolF (a :: Type) = LitBoolF' Bool
    deriving (Show, Eq, Ord, Functor)
-- | Empty list literal: ()
data LitEmptyF (a :: Type) = LitEmptyF'
    deriving (Show, Eq, Ord, Functor)
-- | Pair literal: (a . b)
-- Note that '(1 2 3) produces LitEmpty and LitPair
data LitPairF a = LitPairF' !a !a
    deriving (Show, Eq, Ord, Functor)
-- | Vector literal: #(a b c)
newtype LitVectorF a = LitVectorF' (Vector a)
    deriving (Show, Eq, Ord, Functor)
-- | Bytevector literal: #u8(1 2 3)
newtype LitByteVecF (a :: Type) = LitByteVecF' (Vector Word8)
    deriving (Show, Eq, Ord, Functor)

derive [deriveOpenADT (drop 3), deriveShow1, deriveEq1, deriveOrd1]
    [ ''LitNumF, ''LitStrF, ''LitSymF, ''LitCharF, ''LitBoolF
    , ''LitEmptyF, ''LitPairF, ''LitVectorF, ''LitByteVecF
    ]

-- * Expressions
-- | Literals
-- The literal changes depending on the phase
newtype ExpLitF lit (a :: Type) = ExpLitF' lit
    deriving (Show, Eq, Ord, Functor)
-- | Generic variable reference
newtype ExpVarF name (a :: Type) = ExpVarF' name
    deriving (Show, Eq, Ord, Functor)
-- | Local variable reference
newtype ExpVarLocalF name (a :: Type) = ExpVarLocalF' name
    deriving (Show, Eq, Ord, Functor)
-- | Global variable reference
newtype ExpVarGlobalF (a :: Type) = ExpVarGlobalF' Text
    deriving (Show, Eq, Ord, Functor)
-- | Generic set expression
data ExpSetF name a = ExpSetF' !name !a
    deriving (Show, Eq, Ord, Functor)
-- | Local set expression
data ExpSetLocalF name a = ExpSetLocalF' !name !a
    deriving (Show, Eq, Ord, Functor)
-- | Global set expression
data ExpSetGlobalF a = ExpSetGlobalF' !Text !a
    deriving (Show, Eq, Ord, Functor)
-- | If expression
data ExpIfF a = ExpIfF' !a !a !a
    deriving (Show, Eq, Ord, Functor)
-- | While expression
data ExpWhileF a = ExpWhileF' !a !a
    deriving (Show, Eq, Ord, Functor)
-- | Begin expression
newtype ExpBeginF a = ExpBeginF' (Vector a)
    deriving (Show, Eq, Ord, Functor)
-- | Generic apply expression
data ExpApplyF a = ExpApplyF' !a !(Vector a)
    deriving (Show, Eq, Ord, Functor)
-- | Application of a primitive
data ExpApplyPrimF a = ExpApplyPrimF' !(SomePrim a ())
    deriving (Show, Eq, Ord)
instance Functor ExpApplyPrimF where
    fmap f (ExpApplyPrimF' p) = ExpApplyPrimF' (first f p)
-- | Application of a name
data ExpApplyNameF name a = ExpApplyNameF' !name !(Vector a)
    deriving (Show, Eq, Ord, Functor)
-- | Generalized let expression
-- Paramatrized over the possible let kind
data ExpLetF lk a = ExpLetF' !lk !(Vector (Text, a)) !a
    deriving (Show, Eq, Ord, Functor)
-- | Lambda expression
data ExpLambdaF a = ExpLambdaF' !(Vector Text) !a
    deriving (Show, Eq, Ord, Functor)

instance Show1 ExpApplyPrimF where
    liftShowsPrec sp sl d (ExpApplyPrimF' p) = showParen (d > 10) $
        showString "ExpApplyPrimF' " . liftShowsPrec2 sp sl showsPrec showList 11 p
instance Eq1 ExpApplyPrimF where
    liftEq eq (ExpApplyPrimF' p1) (ExpApplyPrimF' p2) =
        liftEq2 eq (==) p1 p2
instance Ord1 ExpApplyPrimF where
    liftCompare cmp (ExpApplyPrimF' p1) (ExpApplyPrimF' p2) =
        liftCompare2 cmp compare p1 p2
derive [deriveOpenADT (drop 3), deriveShow1, deriveEq1, deriveOrd1]
    [ ''ExpLitF, ''ExpVarF, ''ExpVarLocalF, ''ExpVarGlobalF
    , ''ExpSetF, ''ExpSetLocalF, ''ExpSetGlobalF, ''ExpIfF, ''ExpWhileF
    , ''ExpBeginF, ''ExpApplyF, ''ExpApplyNameF, ''ExpLetF, ''ExpLambdaF
    ]
deriveOpenADT (drop 3) ''ExpApplyPrimF

-- * Statements

data StmtValF name exp (a :: Type) = StmtValF' !name !exp
    deriving (Show, Eq, Ord, Functor)

data StmtDefineF name exp (a :: Type) = StmtDefineF' !name !(Vector Text) !exp
    deriving (Show, Eq, Ord, Functor)

newtype StmtExpF exp (a :: Type) = StmtExpF' exp
    deriving (Show, Eq, Ord, Functor)

data StmtCheckExpectF exp (a :: Type) = StmtCheckExpectF' !exp !Text !exp !Text
    deriving (Show, Eq, Ord, Functor)

data StmtCheckAssertF exp (a :: Type) = StmtCheckAssertF' !exp !Text
    deriving (Show, Eq, Ord, Functor)

derive [deriveOpenADT (drop 4), deriveShow1, deriveEq1, deriveOrd1]
    [ ''StmtValF, ''StmtDefineF, ''StmtExpF
    , ''StmtCheckExpectF, ''StmtCheckAssertF
    ]
