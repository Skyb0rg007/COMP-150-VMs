
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- {-# LANGUAGE UndecidableInstances  #-}
-- {-# LANGUAGE ViewPatterns          #-}

module Type.VariantF
    ( VariantF
    , pattern FV
    ) where

import           Data.Functor.Foldable (Base)
import           Data.Kind             (Type)
import           Type.Variant
import           Type.ListUtils

newtype VariantF (xs :: [t -> Type]) (e :: t) = VariantF (V (ApplyAll e xs))

type family ApplyAll (e :: t) (xs :: [t -> k]) :: [k] where
    ApplyAll e '[]      = '[]
    ApplyAll e (f : fs) = f e : ApplyAll e fs

type instance Base (VariantF xs a) = VariantF xs

pattern FV :: forall c cs e. c :< ApplyAll e cs => c -> VariantF cs e
pattern FV x = VariantF (V x)

