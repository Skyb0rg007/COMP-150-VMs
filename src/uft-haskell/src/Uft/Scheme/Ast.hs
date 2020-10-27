
module Uft.Scheme.Ast
    ( 
    ) where

import           Type.OpenADT
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Text (Text)
import           Data.Word (Word8)
import           Data.Kind (Type)

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
    deriving (Show, Eq, Ord, Functor, Foldable)
data LitNilF (a :: Type) = LitNilF'
    deriving (Show, Eq, Ord, Functor, Foldable)
-- | Pair literal: (a . b)
-- Note that '(1 2 3) produces LitEmpty and LitPair
data LitPairF a = LitPairF' !a !a
    deriving (Show, Eq, Ord, Functor, Foldable)
-- | Vector literal: #(a b c)
newtype LitVectorF a = LitVectorF' (Vector a)
    deriving (Show, Eq, Ord, Functor, Foldable)
-- | Bytevector literal: #u8(1 2 3)
newtype LitByteVecF (a :: Type) = LitByteVecF' (Vector Word8)
    deriving (Show, Eq, Ord, Functor, Foldable)

type LitRowF =
    '[ LitNilF
     , LitPairF
     , LitVectorF
     , LitByteVecF
     ]
type LitF = Sum LitRowF
type Lit = OpenADT LitRowF

pattern LitNil :: LitNilF :< r => OpenADT r
pattern LitNil <- (project . unfix -> Just LitNilF')
    where LitNil = Fix (inject LitNilF')

pattern LitNilF :: LitNilF :< r => Sum r a
pattern LitNilF <- (project -> Just LitNilF')
    where LitNilF = inject LitNilF'

x :: Lit
x = Fix $ inject $ LitVectorF' $ Vector.fromList [Fix $ inject LitNilF', LitNil]

numNils :: forall r. (LitRowF :<: r, Apply Functor r, Apply Foldable r)
        => OpenADT r
        -> Int
numNils = cata alg where
    alg :: Sum r Int -> Int
    alg LitNilF = 1
    alg x = sum x

nilToEmpty :: forall r. (LitNilF :< r, LitEmptyF :< r, Apply Functor r)
           => OpenADT r
           -> OpenADT r
nilToEmpty = cata alg where
    alg :: Sum r (OpenADT r) -> OpenADT r
    alg LitNilF = Fix (inject LitEmptyF')
    alg x = Fix x

