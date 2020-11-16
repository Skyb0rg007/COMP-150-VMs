
module Data.SumSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Type.OpenADT
import Data.Functor.Const

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "decompose'" $ do
        it "Decomposes two types" $
            let x, y, z :: Sum '[Either Char, Maybe, (->) Bool] Int
                x = Sum (Just 12)
                y = Sum (\b -> if b then 1 else 0)
                z = Sum (Left 'a')
                f :: Int -> Sum '[Either Char, Maybe, (->) Bool] Int -> Bool
                f n x = case decompose' @'[Maybe, (->) Bool] x of
                          L1 (Sum (y :: Maybe Int)) -> n == 0
                          L1 (Sum (y :: Bool -> Int)) -> n == 1
                          R1 (s :: Sum '[Either Char] Int) -> n == 2
             in f 0 x && f 1 y && f 2 z
        it "can be reordered" $
            let x :: Sum '[Either Char, Maybe] Int
                x = Sum (Left 'a')
                y :: Sum '[Const Bool, Maybe, Either Char, Const ()] Int
                y = reorder x
             in y `shouldBe` Sum (Left 'a')
