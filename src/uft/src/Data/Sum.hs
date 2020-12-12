{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-
   Module:      Data.Sum
   Description: Open sum of products
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This implementation is taken from the `fastsum` package: https://github.com/patrickt/fastsum

   TODO: change 30 -> 200
   Currently 30 due to slow compilation of this file otherwise
-}

module Data.Sum
    (
    -- * The fundamental sum-of-products type
      Sum (Sum)
    , _Sum
    -- * Operating on sums
    , decompose
    , decompose'
    , diversify
    -- * Typeclass application
    , type (:<)
    , type (:<:)
    , Apply (apply, coapply)
    , Applies
    -- * Reexports
    , module GHC.Generics

    -- * Operating on known sums
    -- , decomposeFirst
    -- , decomposeObvious
    -- , decomposeAbsurd
    -- , weaken
    -- , weaken'
    -- , weaken2
    -- , weaken3
    -- , weaken4
    -- , reorder
    -- * Miscellaneous functions
    -- , elemIndex
    -- * Membership
    -- , Element
    -- , Elements
    -- , apply'
    -- , apply2
    -- , apply2'
    ) where

import           Control.Applicative  (Alternative (empty))
import           Control.Lens         (Prism', prism')
import           Data.Foldable        (asum)
import           Data.Functor.Classes
import           Data.Hashable        (Hashable (..))
import           Data.Hashable.Lifted (Hashable1 (..), hashWithSalt1)
import           Data.Kind            (Constraint, Type)
import           Data.Maybe           (fromMaybe)
import           Data.Proxy           (Proxy (Proxy))
import           Data.Sum.TH          (mkApplyInstance, mkElemIndexTypeFamily)
import           GHC.Exts             (Any, Proxy#, proxy#)
import           GHC.TypeLits         (KnownNat, natVal')
import           Unsafe.Coerce        (unsafeCoerce)
import           GHC.Generics         (type (:+:) (..))
import           Text.Read            (Read (readPrec))
import           Data.Type.List       (Delete, type (\\), type (++), Length)
import           Data.Constraint      (Dict (Dict), (\\), type (:-) (Sub))
import           Data.Constraint.Unsafe

-- Generates:
-- type family ElemIndex (t :: Type -> Type) (ts :: [Type -> Type]) :: Nat where
--   ElemIndex t0 (t0 : _) = 0
--   ElemIndex t1 (t0 : t1 : _) = 1
--   ElemIndex t2 (t0 : t1 : t2 : _) = 2
--   ElemIndex t3 (t0 : t1 : t2 : t3 : _) = 3
--   ElemIndex t4 (t0 : t1 : t2 : t3 : t4 : _) = 4
--   etc...
--   ElemIndex t ts = TypeError (Text "'" :<>: ShowType t :<>: Text "' is not a member of type type-level list" :$$: ShowType ts)
mkElemIndexTypeFamily 30

-- | Sum type over a type-level list of products
data Sum (r :: [Type -> Type]) (a :: Type) where
    Sum' :: {-# UNPACK #-} !Word -> Any -> Sum r v

-- | Bidirectional pattern for constructing and decomposing 'Sum's
pattern Sum :: forall f r a. (f :< r) => f a -> Sum r a
pattern Sum x <- (project -> Just x)
    where Sum x = inject x

-- | Prism for constructing and decomposing 'Sum's
_Sum :: forall f r a. (f :< r) => Prism' (Sum r a) (f a)
_Sum = prism' inject project

inject :: forall f r a. (f :< r) => f a -> Sum r a
inject v = Sum' (unP (elemNo :: P f r)) (unsafeCoerce v)
{-# INLINE inject #-}

project :: forall f r a. (f :< r) => Sum r a -> Maybe (f a)
project (Sum' n x) =
    let n' = unP (elemNo :: P f r)
     in if n == n'
           then Just (unsafeCoerce x)
           else Nothing
{-# INLINE project #-}

-----------------------------------------------------------------------------

-- | Ensure that a type is an element of the type-list at a known position
class KnownNat (ElemIndex f r) => Element r f
instance KnownNat (ElemIndex f r) => Element r f 

-- | Infix 'Element'
infixr 5 :<
type f :< r = Element r f

-- | Ensure that each type is a member of the type-list
type family Elements r (fs :: [Type -> Type]) :: Constraint where
    Elements r (f : fs) = (Element r f, Elements r fs)
    Elements r '[]      = ()

-- | Infix 'Elements'
infixr 5 :<:
type fs :<: r = Elements r fs

-- | Newtype wrapper around the position index
-- Allows for safer access of the element's position
newtype P (f :: Type -> Type) (r :: [Type -> Type]) = P { unP :: Word }

-- | Access the type's position in the type-list
elemNo :: forall f r. (f :< r) => P f r
elemNo = P $ fromIntegral $ natVal' (proxy# :: Proxy# (ElemIndex f r))

-- | Low-level function for accessing the index of the current element in the type-list
elemIndex :: Sum r a -> Word
elemIndex (Sum' n _) = n
{-# INLINE elemIndex #-}

-- | Pattern-match on a type in the 'Sum'
decompose :: forall f r a. (f :< r)
          => Sum r a
          -> (f :+: Sum (Delete f r)) a
decompose (Sum' n v) =
    let n' = unP (elemNo :: P f r)
     in case compare n n' of
          EQ -> L1 (unsafeCoerce v)
          LT -> R1 (Sum' n v)
          GT -> R1 (Sum' (n - 1) v)

class (ts :<: r) => ElemNos ts r where
    elemNos' :: [Word]

instance ElemNos '[] r where
    elemNos' = []

instance (t :< r, ElemNos ts r) => ElemNos (t:ts) r where
    elemNos' = unP (elemNo @t @r) : elemNos' @ts @r

decompose' :: forall rem r a. (ElemNos rem r)
           => Sum r a
           -> (Sum rem :+: Sum (r \\ rem)) a
decompose' (Sum' n v) =
    let f :: Word -> Word -> Either Word Word
        f idx x = case compare n x of
                    EQ -> Left idx
                    LT -> Right 0
                    GT -> Right 1
     in case sequence (zipWith f [0..] (elemNos' @rem @r)) of
          Left idx -> L1 $ Sum' idx v
          Right ns -> R1 $ Sum' (n - sum ns) v
{-# INLINE decompose' #-}

-- | Add an arbitrary product to the 'Sum'
weaken :: Sum r v -> Sum (any : r) v
weaken (Sum' n x) = Sum' (n + 1) x
{-# INLINE weaken #-}

weaken' :: forall r' r v. Sum r v -> Sum (r ++ r') v
weaken' (Sum' n x) = Sum' n x
{-# INLINE weaken' #-}

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
    coapply :: Alternative m
            => (forall g. c g => m (g b))
            -> m (Sum fs b)

instance Apply c '[] where
    apply _ x = decomposeAbsurd x
    coapply _ = empty
    {-# INLINABLE apply #-}
    {-# INLINABLE coapply #-}

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

-- diversify :: forall r r' a. () => Sum r a -> Sum r' a
-- diversity = 

diversify :: forall r' r a. Apply (Element r') r => Sum r a -> Sum r' a
diversify = apply @(Element r') Sum

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

-- Generates:
-- instance c f0               => Apply c '[f0] where ...
-- instance (c f0, c f1)       => Apply c '[f0, f1] where ...
-- instance (c f0, c f1, c f2) => Apply c '[f0, f1, f2] where ...
-- ...
pure (mkApplyInstance <$> [1 .. 30])

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

instance Apply Read1 fs => Read1 (Sum fs) where
    liftReadPrec rp rl = coapply @Read1 (liftReadPrec rp rl)
instance (Apply Read1 fs, Read a) => Read (Sum fs a) where
    readPrec = readPrec1

instance Apply Hashable1 fs => Hashable1 (Sum fs) where
    liftHashWithSalt h salt u = hashWithSalt salt $
        apply @Hashable1 (liftHashWithSalt h (fromIntegral (elemIndex u))) u
instance (Apply Hashable1 fs, Hashable a) => Hashable (Sum fs a) where
    hashWithSalt = hashWithSalt1

type family Applies (cs :: [(Type -> Type) -> Constraint]) (fs :: [Type -> Type]) :: Constraint where
    Applies '[] fs = ()
    Applies (c : cs) fs = (Apply c fs, Applies cs fs)

