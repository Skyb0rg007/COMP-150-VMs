{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-
   Module:      Data.Sum
   Description: Open sum of products
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This implementation is taken from the `fastsum` package: https://github.com/patrickt/fastsum

   TODO: change 50 -> 200
   Currently 50 due to slow compilation of this file otherwise
-}

module Data.Sum
    (
    -- * The fundamental sum-of-products type
      Sum (Sum)
    -- * Operating on sums
    , decompose
    , decompose2
    , decompose3
    , decompose4
    -- * Operating on known sums
    , decomposeFirst
    , decomposeObvious
    , decomposeAbsurd
    , weaken
    , weaken2
    , weaken3
    , weaken4
    -- * Miscellaneous functions
    , elemIndex
    -- * Membership
    , Element
    , type (:<)
    , Elements
    , type (:<:)
    -- * Typeclass application
    , Apply (apply)
    , apply'
    , apply2
    , apply2'
    , Applies
    -- * Reexports
    , module GHC.Generics
    ) where

import           Data.Functor.Classes
import           Data.Hashable        (Hashable (..))
import           Data.Hashable.Lifted (Hashable1 (..), hashWithSalt1)
import           Data.Kind            (Constraint, Type)
import           Data.Maybe           (fromMaybe)
import           Data.Sum.TH          (mkApplyInstance, mkElemIndexTypeFamily)
import           GHC.Exts             (Any, Proxy#, proxy#)
import           GHC.TypeLits         (KnownNat, natVal')
import           Unsafe.Coerce        (unsafeCoerce)
import           GHC.Generics         (type (:+:) (..))
import           Type.List            (Delete, type (\\), type (++))

-- Generates:
-- type family ElemIndex (t :: Type -> Type) (ts :: [Type -> Type]) :: Nat where
--   ElemIndex t0 (t0 : _) = 0
--   ElemIndex t1 (t0 : t1 : _) = 1
--   ElemIndex t2 (t0 : t1 : t2 : _) = 2
--   ElemIndex t3 (t0 : t1 : t2 : t3 : _) = 3
--   ElemIndex t4 (t0 : t1 : t2 : t3 : t4 : _) = 4
--   etc...
--   ElemIndex t ts = TypeError (Text "'" :<>: ShowType t :<>: Text "' is not a member of type type-level list" :$$: ShowType ts)
mkElemIndexTypeFamily 100

-- | Sum type over a type-level list of products
data Sum (r :: [Type -> Type]) (v :: Type) where
    Sum' :: {-# UNPACK #-} !Word -> Any -> Sum r v

-- | Bidirectional pattern for constructing and decomposing 'Sum's
pattern Sum :: forall e r v. (e :< r) => e v -> Sum r v
pattern Sum x <- (project -> Just x)
    where Sum x = inject x

inject :: forall e r v. (e :< r) => e v -> Sum r v
inject v = Sum' (unP (elemNo :: P e r)) (unsafeCoerce v)
{-# INLINE inject #-}

project :: forall e r v. (e :< r) => Sum r v -> Maybe (e v)
project (Sum' n x) =
    let n' = unP (elemNo :: P e r)
     in if n == n'
           then Just (unsafeCoerce x)
           else Nothing
{-# INLINE project #-}

-----------------------------------------------------------------------------

-- | Ensure that a type is an element of the type-list at a known position
type Element t r = KnownNat (ElemIndex t r)

-- | Infix 'Element'
infixr 5 :<
type t :< r = Element t r

-- | Ensure that each type is a member of the type-list
type family Elements (ts :: [Type -> Type]) r :: Constraint where
    Elements (t : ts) r = (Element t r, Elements ts r)
    Elements '[] r = ()

-- | Infix 'Elements'
infixr 5 :<:
type ts :<: r = Elements ts r

-- | Newtype wrapper around the position index
-- Allows for safer access of the element's position
newtype P (t :: Type -> Type) (r :: [Type -> Type]) = P { unP :: Word }

-- | Access the type's position in the type-list
elemNo :: forall t r. (t :< r) => P t r
elemNo = P $ fromIntegral $ natVal' (proxy# :: Proxy# (ElemIndex t r))

-- elemNo' :: forall t r. (t :< r) => Word
-- elemNo' = unP (elemNo :: P t r)

-- elemNos :: forall ts r. (ts :<: r) => [Word]
-- elemNos = elemNo

-- | Low-level function for accessing the index of the current element in the type-list
elemIndex :: Sum r v -> Word
elemIndex (Sum' n _) = n
{-# INLINE elemIndex #-}

-- decompose' :: forall r r' b. (r' :<: r)
           -- => Sum r b
           -- -> (Sum r' :+: Sum (r \\ r')) b
-- decompose' (Sum' n v) = undefined
    -- let 

-- | Pattern-match on a type in the 'Sum'
decompose :: forall e es b. (e :< es)
          => Sum es b
          -> (e :+: Sum (Delete e es)) b
decompose (Sum' n v) =
    let n' = unP (elemNo :: P e es)
     in case compare n n' of
          EQ -> L1 (unsafeCoerce v)
          LT -> R1 (Sum' n v)
          GT -> R1 (Sum' (n - 1) v)

decompose2 :: forall a b es x. ('[a, b] :<: es)
           => Sum es x
           -> (Sum '[a, b] :+: Sum (es \\ '[a, b])) x
           -- -> (a :+: b :+: Sum (es \\ '[a, b])) x
decompose2 (Sum' n v) =
    let a = unP (elemNo :: P a es)
        b = unP (elemNo :: P b es)
     in case (compare n a, compare n b) of
          (EQ, _) -> L1 $ Sum' 0 v
          (_, EQ) -> L1 $ Sum' 1 v
          (x, y)  ->
              let offset GT = 1
                  offset _  = 0
               in R1 $ Sum' (n - offset x - offset y) v

decompose3 :: forall a b c es x. ('[a, b, c] :<: es)
           => Sum es x
           -> (a :+: b :+: c :+: Sum (es \\ '[a, b, c])) x
decompose3 (Sum' n v) =
    let a = unP (elemNo :: P a es)
        b = unP (elemNo :: P b es)
        c = unP (elemNo :: P c es)
     in case (compare n a, compare n b, compare n c) of
          (EQ, _, _) -> L1 (unsafeCoerce v :: a x)
          (_, EQ, _) -> R1 (L1 (unsafeCoerce v :: b x))
          (_, _, EQ) -> R1 (R1 (L1 (unsafeCoerce v :: c x)))
          (x, y, z)  ->
              let offset GT = 1
                  offset _  = 0
               in R1 (R1 (R1 (Sum' (n - offset x - offset y - offset z) v)))

decompose4 :: forall a b c d es x. ('[a, b, c, d] :<: es)
           => Sum es x
           -> (a :+: b :+: c :+: d :+: Sum (es \\ '[a, b, c, d])) x
decompose4 (Sum' n v) =
    let a = unP (elemNo :: P a es)
        b = unP (elemNo :: P b es)
        c = unP (elemNo :: P c es)
        d = unP (elemNo :: P d es)
     in case (compare n a, compare n b, compare n c, compare n d) of
          (EQ, _, _, _) -> L1 (unsafeCoerce v :: a x)
          (_, EQ, _, _) -> R1 (L1 (unsafeCoerce v :: b x))
          (_, _, EQ, _) -> R1 (R1 (L1 (unsafeCoerce v :: c x)))
          (_, _, _, EQ) -> R1 (R1 (R1 (L1 (unsafeCoerce v :: d x))))
          (x, y, z, zz) ->
              let offset GT = 1
                  offset _  = 0
               in R1 (R1 (R1 (R1 (Sum' (n - offset x - offset y - offset z - offset zz) v))))

-- | Pattern-match on the first type in the 'Sum'
decomposeFirst :: Sum (e : es) b -> (e :+: Sum es) b
decomposeFirst sum@(Sum' n v) =
    case project sum of
      Just x  -> L1 x
      Nothing -> R1 (Sum' (n - 1) v)
{-# INLINE decomposeFirst #-}

-- | A 'Sum' type with only one variant must be that variant
decomposeObvious :: Sum '[e] b -> e b
decomposeObvious (Sum' _ v) = unsafeCoerce v
{-# INLINE decomposeObvious #-}

-- | A 'Sum' type with no variants should be impossible to construct
decomposeAbsurd :: Sum '[] b -> a
decomposeAbsurd (Sum' _ _) = error "Data.Sum.decomposeAbsurd: empty variant!"

-- | Add an arbitrary product to the 'Sum'
weaken :: Sum r v -> Sum (any : r) v
weaken (Sum' n x) = Sum' (n + 1) x
{-# INLINE weaken #-}

weaken2 :: Sum r v -> Sum (any1 : any2 : r) v
weaken2 (Sum' n x) = Sum' (n + 2) x
{-# INLINE weaken2 #-}

weaken3 :: Sum r v -> Sum (any1 : any2 : any3 : r) v
weaken3 (Sum' n x) = Sum' (n + 3) x
{-# INLINE weaken3 #-}

weaken4 :: Sum r v -> Sum (any1 : any2 : any3 : any4 : r) v
weaken4 (Sum' n x) = Sum' (n + 4) x
{-# INLINE weaken4 #-}

class Apply (c :: (Type -> Type) -> Constraint) (fs :: [Type -> Type]) where
    apply :: (forall g. c g => g a -> b) -> Sum fs a -> b

instance Apply c '[] where
    apply _ x = decomposeAbsurd x

-- | 'apply', but given a function for lifting the value back into the sum
apply' :: forall c fs a b . (Apply c fs)
       => (forall g . c g => (forall x. g x -> Sum fs x) -> g a -> b)
       -> Sum fs a
       -> b
apply' f u@(Sum' n _) = apply @c (f (Sum' n . unsafeCoerce)) u
{-# INLINABLE apply' #-}

-- | 'apply' but operating on two sums at once
-- Returns 'Nothing' unless both 'Sum' variants are the same
apply2 :: forall c fs a b d . (Apply c fs)
       => (forall g . c g => g a -> g b -> d)
       -> Sum fs a
       -> Sum fs b -> Maybe d
apply2 f u@(Sum' n1 _) (Sum' n2 r2)
  | n1 == n2  = Just (apply @c (\r1 -> f r1 (unsafeCoerce r2)) u)
  | otherwise = Nothing
{-# INLINABLE apply2 #-}

-- | 'apply2'' but given a function for lifting the value back into the sum
apply2' :: forall c fs a b d . (Apply c fs)
        => (forall g . c g => (forall x. g x -> Sum fs x) -> g a -> g b -> d)
        -> Sum fs a
        -> Sum fs b
        -> Maybe d
apply2' f u@(Sum' n1 _) (Sum' n2 r2)
  | n1 == n2  = Just (apply' @c (\reinject r1 -> f reinject r1 (unsafeCoerce r2)) u)
  | otherwise = Nothing
{-# INLINABLE apply2' #-}

-- Generates:
-- instance c f0               => Apply c '[f0] where apply = ...
-- instance (c f0, c f1)       => Apply c '[f0, f1] where apply = ...
-- instance (c f0, c f1, c f2) => Apply c '[f0, f1, f2] where apply = ...
-- ...
pure (mkApplyInstance <$> [1 .. 100])

instance Apply Foldable fs => Foldable (Sum fs) where
    foldMap f = apply @Foldable (foldMap f)
    {-# INLINABLE foldMap #-}
    foldr f acc = apply @Foldable (foldr f acc)
    {-# INLINABLE foldr #-}
    foldl f acc = apply @Foldable (foldl f acc)
    {-# INLINABLE foldl #-}
    null = apply @Foldable null
    {-# INLINABLE null #-}
    length = apply @Foldable length
    {-# INLINABLE length #-}

instance Apply Functor fs => Functor (Sum fs) where
    fmap f = apply' @Functor (\r -> r . fmap f)
    {-# INLINABLE fmap #-}
    (<$) x = apply' @Functor (\r -> r . (<$) x)
    {-# INLINABLE (<$) #-}

instance (Apply Functor fs, Apply Foldable fs, Apply Traversable fs) => Traversable (Sum fs) where
    traverse f = apply' @Traversable (\r -> fmap r . traverse f)
    {-# INLINABLE traverse #-}
    sequenceA = apply' @Traversable (\r -> fmap r . sequenceA)
    {-# INLINABLE sequenceA #-}

instance Apply Eq1 fs => Eq1 (Sum fs) where
    liftEq eq u1 u2 = fromMaybe False $ apply2 @Eq1 (liftEq eq) u1 u2
instance (Apply Eq1 fs, Eq a) => Eq (Sum fs a) where
    (==) = eq1

instance (Apply Eq1 fs, Apply Ord1 fs) => Ord1 (Sum fs) where
    liftCompare cmp u1 u2 = fromMaybe (compare (elemIndex u1) (elemIndex u2)) $
        apply2 @Ord1 (liftCompare cmp) u1 u2
instance (Apply Eq1 fs, Apply Ord1 fs, Ord a) => Ord (Sum fs a) where
    compare = compare1

instance Apply Show1 fs => Show1 (Sum fs) where
    liftShowsPrec sp sl d = apply @Show1 (liftShowsPrec sp sl d)
instance (Apply Show1 fs, Show a) => Show (Sum fs a) where
    showsPrec = showsPrec1

instance Apply Hashable1 fs => Hashable1 (Sum fs) where
    liftHashWithSalt h salt u = hashWithSalt salt $
        apply @Hashable1 (liftHashWithSalt h (fromIntegral (elemIndex u))) u
instance (Apply Hashable1 fs, Hashable a) => Hashable (Sum fs a) where
    hashWithSalt = hashWithSalt1

type family Applies (cs :: [(Type -> Type) -> Constraint]) (fs :: [Type -> Type]) :: Constraint where
    Applies '[] fs = ()
    Applies (c : cs) fs = (Apply c fs, Applies cs fs)

-- broaden
    -- :: forall r1 r2 x. (r1 :<: r2)
    -- => Sum r1
    -- -> Sum r2
-- broaden s
