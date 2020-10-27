
module Type.OpenADT
    ( OpenADT
    , module Data.Sum
    , module Data.Functor.Foldable
    ) where

import           Data.Sum
import           Data.Functor.Foldable (Fix (..), unfix, cata)

type OpenADT r = Fix (Sum r)

