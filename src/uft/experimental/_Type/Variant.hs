
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
    ( V
    , pattern V
    , (:<)
    , (:<<)
    ) where

import           Control.Monad (guard)
import           Data.Kind     (Type, Constraint)
import           Data.Proxy    (Proxy (..))
import           Data.Void     (Void)
import           Data.Word     (Word)
import           GHC.Exts      (Any)
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)

import           Type.ListUtils

type role V representational
data V (l :: [Type]) = Variant {-# UNPACK #-} !Word !Any

instance Eq (V '[]) where
    _ == _ = True

instance (Eq (V xs), Eq x) => Eq (V (x : xs)) where
    v1@(Variant t1 _) == v2@(Variant t2 _)
      | t1 /= t2 = False
      | otherwise =
          case (popVariantHead v1, popVariantHead v2) of
            (Right a, Right b) -> a == b
            (Left as, Left bs) -> as == bs
            _                  -> False

instance Ord (V '[]) where
    compare _ _ = error "Empty variant"

instance (Ord (V xs), Ord x) => Ord (V (x : xs)) where
    compare v1 v2 =
        case (popVariantHead v1, popVariantHead v2) of
          (Right a, Right b) -> compare a b
          (Left as, Left bs) -> compare as bs
          (Right _, Left _) -> LT
          (Left _, Right _) -> GT

instance Show (V '[]) where
    showsPrec _ _ = error "Empty variant"

instance (Show (V xs), Show x) => Show (V (x : xs)) where
    showsPrec d v =
        case popVariantHead v of
          Right x -> showsPrec d x
          Left xs -> showsPrec d xs

pattern V :: forall c cs. (c :< cs) => c -> V cs
pattern V x <- (fromVariant -> Just x)
    where V x = toVariant x

-- 'popVariantHead v' returns a value if the variant contains a value of the
-- first type in the type list. Otherwise, it returns a variant without that type.
popVariantHead :: forall x xs. V (x : xs) -> Either (V xs) x
popVariantHead v@(Variant t a) =
    case fromVariantAt @0 v of
      Just x -> Right x
      Nothing -> Left $ Variant (t - 1) a

fromVariantAt
    :: forall (n :: Nat) (l :: [Type]). KnownNat n
    => V l
    -> Maybe (Index n l)
fromVariantAt (Variant t a) = do
    guard (t == fromInteger (natVal (Proxy :: Proxy n)))
    pure $ unsafeCoerce a

toVariantAt
    :: forall (n :: Nat) (l :: [Type]). KnownNat n
    => Index n l
    -> V l
toVariantAt a = Variant (fromInteger (natVal (Proxy :: Proxy n))) (unsafeCoerce a)

type (:<?) x xs = PopVariant x xs

class PopVariant a xs where
    popVariant' :: V xs -> Either (V (Remove a xs)) a

instance PopVariant a '[] where
    popVariant' _ = error "Empty variant"

instance forall a xs n xs' y ys.
    ( PopVariant a xs'
    , n ~ MaybeIndexOf a xs
    , xs' ~ RemoveAt1 n xs
    , Remove a xs' ~ Remove a xs
    , KnownNat n
    , xs ~ (y : ys)
    )
  => PopVariant a (y : ys) where
    popVariant' (Variant t a) =
        case fromInteger $ natVal (Proxy :: Proxy n) of
          0 -> Left (Variant t a)
          n | n - 1 == t -> Right (unsafeCoerce a)
            | n - 1 < t  -> popVariant' @a @xs' (Variant (t - 1) a)
            | otherwise -> Left (Variant t a)

popVariant :: forall a xs. (a :< xs) => V xs -> Either (V (Remove a xs)) a
popVariant v = popVariant' @a v

toVariant :: forall a l. (a :< l) => a -> V l
toVariant = toVariantAt @(IndexOf a l)

type (:<) x xs =
    ( CheckMember x xs
    , Member x xs
    , x :<? xs
    )

fromVariant :: forall a xs. (a :< xs) => V xs -> Maybe a
fromVariant v =
    case popVariant v of
      Right a -> Just a
      Left _  -> Nothing

type family (:<<) xs ys :: Constraint where
    '[]      :<< ys = ()
    (x : xs) :<< ys = (x :< ys, xs :<< ys)

