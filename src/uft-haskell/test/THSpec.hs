
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -ddump-splices #-}

module THSpec (main, spec) where

import           Control.Monad         (forM_)
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Functor.Foldable
-- import           Data.Row
-- import qualified Data.Row.Variants     as V
import           Data.Char             (toLower)
import           Data.Kind             (Type)
import           Generic.Data
import           GHC.Generics          (Generic, Generic1)
import           Test.Hspec
import           Test.QuickCheck
import           Type.OpenADT
import           Type.OpenADT.TH

openADT (map toLower) [d| 
    data FooF a = FooF' !a !a
    data BarF c b a = BarF' !Int !Bool (Maybe a)
    newtype BazF a = BazF' Int
    |]

-- data FooF (a :: Type) = Foof'
    -- deriving (Functor)
-- openADTBase ''FooF
-- openADT ''FooF "foo" "Foo" "FooF"

-- data Bar a b = Bar a b
-- deriveEq2 ''Bar
-- instance Eq a => Eq1 (Bar a) where
    -- liftEq = liftEq2 (==)

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()

