
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Uft2.DTALC
    ( Summable (..)
    , Summed (..)
    , Projectable (..)
    , Injectable (..)
    , (:<:) (..)
    , pattern X
    ) where

import           Data.Text (Text)
import           Data.Void (Void)
import           Data.Kind (Type)

-- | Summed is a sum type of functors * -> *
class Summable (fs :: [Type -> Type]) where
    data Summed fs :: Type -> Type

instance Summable '[] where
    data Summed '[] a = SummedNil Void
        deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Summable (f ': fs) where
    data Summed (f ': fs) a = Functor f => Here (f a)
                            | There (Summed fs a)

deriving instance (Show (f a), Show (Summed fs a)) => Show (Summed (f ': fs) a)
deriving instance (Eq (f a), Eq (Summed fs a))     => Eq (Summed (f ': fs) a)
deriving instance (Ord (f a), Ord (Summed fs a))   => Ord (Summed (f ': fs) a)
deriving instance Functor (Summed fs) => Functor (Summed (f ': fs))

-- | 'f' is injectible into 'Summed fs'
class Injectable (f :: Type -> Type) (fs :: [Type -> Type]) where
    inj :: f a -> Summed fs a

instance Functor f => Injectable f (f ': fs) where
    inj = Here

instance {-# OVERLAPPABLE #-} Injectable f fs => Injectable f (g ': fs) where
    inj = There . inj

-- | 'Summed fs' is possible projectable into 'f'
class Projectable (f :: Type -> Type) (fs :: [Type -> Type]) where
    prj :: Summed fs a -> Maybe (f a)

instance Projectable f (f ': fs) where
    prj (Here fa) = Just fa
    prj (There _) = Nothing

instance {-# OVERLAPPABLE #-} Projectable f fs => Projectable f (g ': fs) where
    prj (Here _) = Nothing
    prj (There s) = prj s

-- | Exported typeclass
class (Summable fs, Injectable f fs, Projectable f fs, Functor (Summed fs))
  => (f :: Type -> Type) :<: (fs :: [Type -> Type])

instance (Summable fs, Injectable f fs, Projectable f fs, Functor (Summed fs))
  => f :<: fs

-- | Helper pattern for projecting / injecting
pattern X :: f :<: fs => f a -> Summed fs a
pattern X x <- (prj -> Just x)
    where X x = inj x

-- class (Summable fs, Functor (Summed fs)) => All c fs where
    -- mapSummed :: (forall f. (Projectable f fs, c f) => f a -> b) -> Summed fs a -> b


