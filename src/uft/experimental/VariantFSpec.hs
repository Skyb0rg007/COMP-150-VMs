{-# LANGUAGE DeriveFunctor, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VariantFSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Type.VariantF
-- import Haskus.Utils.VariantF

main :: IO ()
main = hspec spec

newtype LitInt a = LitInt Int
    deriving (Functor)
data LitPair a = LitPair !a !a
    deriving (Functor)
data LitNil a = LitNil
    deriving (Functor)

type LiteralF = VariantF '[LitInt, LitPair, LitNil]

spec :: Spec
spec =
    describe "Works with example literals" $ do
        it "Can pattern match on ints" $
            flip shouldBe True $
            let n = FV (LitInt 2 :: LitInt ()) :: LiteralF ()
             in case n of
                  FV (LitInt _ :: LitInt ()) -> True
                  _ -> False
        -- it "Can do recursion-schemes stuff" $
            -- let alg :: LiteralF (LiteralF ()) -> Int
                -- alg = \case
                    -- FV (LitInt _ :: LitInt (LiteralF ())) -> 
