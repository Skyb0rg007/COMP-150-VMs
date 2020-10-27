
module Type.OpenADT
    ( OpenADT
    , Fix (..)
    , unfix
    , module Type.VarF
    , module Data.Row
    ) where

import           Data.Functor.Foldable (Fix (..), unfix)
import           Data.Row
import           Type.VarF             (Label (..), OpenAlg, VarF (..), caseonF,
                                        diversifyF, impossibleF, multiTrialF,
                                        trialF, viewF)

type OpenADT r = Fix (VarF r)

