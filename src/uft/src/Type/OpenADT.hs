
module Type.OpenADT
    ( OpenADT
    ) where

import           Data.Functor.Foldable (Fix)
import           Type.VarF             (VarF)

type OpenADT r = Fix (VarF r)

