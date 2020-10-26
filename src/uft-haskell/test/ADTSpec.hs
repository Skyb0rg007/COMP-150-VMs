
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module ADTSpec (main, spec) where

import           Control.Monad         (forM_)
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Functor.Foldable
-- import           Data.Row
-- import qualified Data.Row.Variants     as V
import           Generic.Data
import           GHC.Generics          (Generic, Generic1)
import           Test.Hspec
import           Test.QuickCheck
import           Type.OpenADT
import           Type.OpenADT.TH
import           Data.Kind (Type)

-- * Types

newtype LitIntF (a :: Type) = LitIntF' Int
    deriving (Functor, Show, Eq, Ord)

data LitPairF a = LitPairF' !a !a
    deriving (Functor, Show, Eq, Ord)

data LitNilF a = LitNilF'
    deriving (Functor, Show, Eq, Ord)

-- * Patterns

pattern LitIntF :: OpenAlg r "litInt" LitIntF v
                => Int
                -> VarF r v
pattern LitIntF n <- (viewF #litInt -> Just (LitIntF' n))
    where LitIntF n = VarF (IsJust #litInt (LitIntF' n))

pattern LitInt :: OpenAlg r "litInt" LitIntF (OpenADT r)
               => Int
               -> OpenADT r
pattern LitInt n <- Fix (VarF (IsJust (Label :: Label "litInt") (LitIntF' n)))
    where LitInt n = Fix (LitIntF n)

pattern LitPairF :: OpenAlg r "litPair" LitPairF v
                => v
                -> v
                -> VarF r v
pattern LitPairF x y <- (viewF #litPair -> Just (LitPairF' x y))
    where LitPairF x y = VarF (IsJust #litPair (LitPairF' x y))

pattern LitPair :: OpenAlg r "litPair" LitPairF (OpenADT r)
               => OpenADT r
               -> OpenADT r
               -> OpenADT r
pattern LitPair x y <- Fix (VarF (IsJust (Label :: Label "litPair") (LitPairF' x y)))
    where LitPair x y = Fix (LitPairF x y)

pattern LitNilF :: OpenAlg r "litNil" LitNilF v
                => VarF r v
pattern LitNilF <- (viewF #litNil -> Just LitNilF')
    where LitNilF = VarF (IsJust #litNil LitNilF')

pattern LitNil :: OpenAlg r "litNil" LitNilF (OpenADT r)
               => OpenADT r
pattern LitNil = Fix LitNilF

-- * Deriving instances

deriveEq1 ''LitIntF
deriveEq1 ''LitPairF
deriveEq1 ''LitNilF

deriveOrd1 ''LitIntF
deriveOrd1 ''LitPairF
deriveOrd1 ''LitNilF

deriveShow1 ''LitIntF
deriveShow1 ''LitPairF
deriveShow1 ''LitNilF

-- * Type synonyms for literals

type LitRowsF =
    ( "litInt"  .== LitIntF
   .+ "litPair" .== LitPairF
   .+ "litNil"  .== LitNilF
    )
type LitF = VarF LitRowsF
type Lit = Fix LitF

-- * Example literals

ex :: Lit
ex = LitPair (LitInt 12) LitNil

ex2 :: Lit
ex2 = LitNil

ex3 :: Lit
ex3 = LitInt 12

ex4 :: Lit
ex4 = LitPair (LitPair (LitInt 1) (LitInt 2)) (LitPair (LitInt 3) (LitInt 4))

allPairs :: [a] -> [(a, a)]
allPairs = go
    where go [] = []
          go (x:xs) = map (x,) xs ++ go xs

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ADT instances works" $ do
        it "Show instance works" $
            show ex `shouldBe` "Fix (VarF (LitPairF' (Fix (VarF (LitIntF' 12))) (Fix (VarF LitNilF'))))"
        it "Eq instance works for equality" $
            ex `shouldBe` ex
        it "Eq instance works for disequality" $
            forM_ (allPairs [ex, ex2, ex3, ex4]) $ \(a, b) ->
                a `shouldNotBe` b
        it "Ord instance works" $
            forM_ (allPairs [ex, ex2, ex3, ex4]) $ \(a, b) ->
                case compare a b of
                  LT -> compare b a `shouldBe` GT
                  EQ -> compare b a `shouldBe` EQ
                  GT -> compare b a `shouldBe` LT
    describe "Morphisms work" $ do
        it "cata works" $
            let alg :: LitF Int -> Int
                alg = caseonF $
                    #litNil  .== (\LitNilF' -> 0)
                 .+ #litInt  .== (\(LitIntF' n) -> n)
                 .+ #litPair .== (\(LitPairF' a b) -> a + b)
             in cata alg ex4 `shouldBe` 10

