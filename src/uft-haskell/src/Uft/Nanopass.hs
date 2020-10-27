
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Uft.Nanopass
    ( Nanopass (..)
    ) where

import           GHC.TypeLits
import           Data.Row
import           Data.Kind
import           Type.OpenADT
import           Type.VarF
import           Data.Functor.Foldable

-- | Typeclass representing a stage in the nanopass architecture
class KnownSymbol name => Nanopass (name :: Symbol) where
    type In name (r :: Row (Type -> Type)) :: Constraint
    type Out name (r :: Row (Type -> Type)) :: Row (Type -> Type)
    type M name (m :: (Type -> Type)) :: Constraint
    type M name m = Monad m
    passName :: Label name
    passName = Label
    pass :: (M name m, In name r) => OpenADT r -> m (OpenADT (Out name r))

