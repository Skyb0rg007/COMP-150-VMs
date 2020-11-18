
module Type.Hyper
    ( Hyper (Hyper)
    , UnHyper
    , type (#)
    , type (:#)
    ) where

import           Data.Kind (Type)
import           GHC.Exts (Any)

-- * Hypertypes

newtype Hyper = Hyper { unHyper :: Hyper -> Type }

type family UnHyper h where
    UnHyper ('Hyper t) = t

type (h :: Hyper -> Type) # (p :: Hyper -> Type) = h ('Hyper p) :: Type

type (h :: Hyper) :# (p :: Hyper -> Type) = UnHyper h # p :: Type

-- * Example

data VarF (h :: Hyper -> Type) = VarF' String

data LetF expr h = LetF' [(String, h :# expr)] (h :# expr)

-- * Hyper Sum

data Sum (r :: [Hyper -> Type]) (x :: Hyper) = Sum' !Word Any

inject :: h # p -> Sum '[h] ('Hyper p)
inject = undefined

