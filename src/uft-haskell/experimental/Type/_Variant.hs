
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
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Type.Variant
    (
    ) where

import           Data.Functor.Foldable        (Base)
import           Data.Kind                    (Constraint, Type)
import           Data.Proxy                   (Proxy (..))
import           Data.Singletons              ()
import           Data.Singletons.Prelude.List ()
-- import           Data.Singletons.Prelude.Num  (type (+), type (-))
-- import           Data.Singletons.TypeLits     (KnownNat, Nat, natVal)
-- import           Data.Singletons.TypeError    (TypeError, ErrorMessage' (..))
import           GHC.TypeLits
import           GHC.Exts                     (Any)
import           Unsafe.Coerce                (unsafeCoerce)
import           Type.ListUtils

-- | Variant type
-- 'V ts' represents one of the types in the type argument
-- Implemented as a tagged union using unsafe coercion from the type and 'Any'
data V (ts :: [Type]) = Variant {-# UNPACK #-} !Word !Any

-- By default, the 'ts' type argument is phantom since it isn't used on the rhs
-- It needs to be representational to prevent 'coerce' from messing up the invariants
type role V representational

-- | Exported pattern synonym for variants
-- This pattern is used to construct values of variant type
-- as well as pattern-match on variants.
-- Due to limitations of GHC's pattern-matching completeness algorithm,
-- patterns that should be complete are likely not marked as such.
pattern V :: t :< ts => t -> V ts
pattern V x <- (fromVariant -> Just x)
    where V x = toVariant x

-- * Implementation

-- Usage: 'fromVariantAt' @n v
-- Tries to access the nth sum of the variant
fromVariantAt
    :: forall n ts. KnownNat n
    => V ts
    -> Maybe (Index n ts)
fromVariantAt (Variant t x) =
    if t == fromIntegral (natVal (Proxy :: Proxy n))
       then Just $ unsafeCoerce x
       else Nothing

-- Usage: 'toVariantAt' @n x
-- Constructs a variant from the value x of type t, where t is the nth type in the variant
toVariantAt
    :: forall n ts. KnownNat n
    => Index n ts
    -> V ts
toVariantAt x =
    Variant (fromIntegral (natVal (Proxy :: Proxy n))) (unsafeCoerce x)

toVariant
    :: forall t ts. t :< ts
    => t
    -> V ts
toVariant = toVariantAt @(IndexOf t ts)

popVariantHead = undefined

type (:<) t ts = (CheckMember t ts, Member t ts, PopVariant t ts)

-- | Class for constructors that are members of the variant
class PopVariant t ts where
    popVariant' :: V ts -> Either (V (Remove t ts)) c

popVariant :: forall t ts. t :< ts => V ts -> Either (V (Remove t ts)) t
popVariant = popVariant' @t

fromVariant :: t :< ts => V ts -> Maybe t
fromVariant v =
    case popVariant v of
      Right x -> Just x
      Left _  -> Nothing

-- | (xs :<< ys) ensures that (∀x∈ xs)(x :< ys)
type family (:<<) xs ys :: Constraint where
    '[]      :<< ys = ()
    (x : xs) :<< ys = (x :< ys, xs :<< ys)

-- * Instances for Prelude typeclasses

instance Eq (V '[]) where
    _ == _ = True

-- instance (Eq (V xs), Eq x) => Eq (V (x : xs)) where
    -- v1@(Variant t1 _) == v2@(Variant t2 _)
      -- | t1 /= t2 = False
      -- | otherwise =
          -- case (popVariantHead v1, popVariantHead v2) of
            -- (Right a, Right b) -> a == b
            -- (Left as, Left bs) -> as == bs
            -- _                  -> False

instance Ord (V '[]) where
    compare _ _ = error "Empty variant"

-- instance (Ord (V xs), Ord x) => Ord (V (x : xs)) where
    -- compare v1 v2 =
        -- case (popVariantHead v1, popVariantHead v2) of
          -- (Right a, Right b) -> compare a b
          -- (Left as, Left bs) -> compare as bs
          -- (Right _, Left _) -> LT
          -- (Left _, Right _) -> GT

-- instance Show (V '[]) where
    -- showsPrec _ _ = error "Empty variant"

-- instance (Show (V xs), Show x) => Show (V (x : xs)) where
    -- showsPrec d v =
        -- case popVariantHead v of
          -- Right x -> showsPrec d x
          -- Left xs -> showsPrec d xs

