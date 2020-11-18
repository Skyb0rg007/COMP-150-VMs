
-- TODO: This module is not ready for use
module Language.Scheme.Numerics
    ( 
    ) where

import           Data.Complex
import           Data.Int     (Int64)
import           GHC.Real
import           Numeric            (showGFloat, showIntAtBase)

data SchemeNum
    = Fixnum !Int64    -- fixnum - represented by int64_t
    | Bignum !Integer  -- bignum - represented by mpz_t
    | Ratnum !Rational -- ratnum - represented by mpq_t
    | Flonum !Double   -- flonum - represented by double
                       -- complex - represented by pair of nums
    | ComplexNum !(Complex SchemeNum) -- Invariant: doesn't contain complex nums
    deriving Eq

instance Show SchemeNum where
    showsPrec _ = \case
        Fixnum n -> shows n
        Bignum n -> shows n
        Ratnum (a :% b) -> shows a . showChar '/' . shows b
        Flonum f 
          | isNaN f      -> showString "+nan.0"
          | isInfinite f -> showString (if f < 0 then "-inf.0" else "+inf.0")
          | otherwise    -> showGFloat Nothing f
        ComplexNum (a :+ b) ->
            let c = if isNegative b then '-' else '+'
             in shows a . showChar c . shows b . showChar 'i'

isNegative :: SchemeNum -> Bool
isNegative (Fixnum n) = n < 0
isNegative (Bignum n) = n < 0
isNegative (Ratnum r) = r < 0
isNegative (Flonum f) = f < 0
isNegative ComplexNum{} = undefined

isZero :: SchemeNum -> Bool
isZero (Fixnum n) = n == 0
isZero (Bignum n) = n == 0
isZero (Ratnum r) = r == 0
isZero (Flonum f) = f == 0
isZero ComplexNum{} = undefined

-- * Numeric type predicates
isNumber, isComplex, isReal, isRational, isInteger :: SchemeNum -> Bool
isNumber _ = True

isComplex _ = True

isReal (ComplexNum (a :+ b)) = isZero b
isReal _                     = True

isRational ComplexNum{} = False
isRational _ = True

isInteger Fixnum{} = True
isInteger Bignum{} = True
isInteger _        = False

-- isReal 

