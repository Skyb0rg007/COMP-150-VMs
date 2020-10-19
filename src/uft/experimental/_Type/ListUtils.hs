
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.ListUtils
    ( Index
    , MaybeIndexOf
    , IndexOf
    , IndexesOf
    , CheckMember
    , Member
    , Remove
    , RemoveAt1
    , ConstrainAll
    ) where

import           Data.Kind (Type, Constraint)
import           GHC.TypeLits

type family (a :: [k]) ++ (b :: [k]) :: [k] where
    '[] ++ ys = ys
    (x : xs) ++ ys = x : xs ++ ys

-- ConstrainAll :: (k -> Constraint) -> [k] -> Constraint
-- Constrains each element of the list
type family ConstrainAll (f :: k -> Constraint) (xs :: [k]) :: Constraint where
    ConstrainAll f '[]      = ()
    ConstrainAll f (x : xs) = (f x, ConstrainAll f xs)

-- Remove :: k -> [k] -> [k]
-- Removes the element from the list
type family Remove (a :: k) (l :: [k]) :: [k] where
    Remove a '[] = '[]
    Remove a (a : as) = Remove a as
    Remove a (b : as) = b : Remove a as

-- RemoveAt :: Nat -> [k] -> [k]
-- Removes the element at the given index (1-indexed)
type family RemoveAt (n :: Nat) (l :: [k]) :: [k] where
    RemoveAt 0 (x : xs) = xs
    RemoveAt n (x : xs) = x : RemoveAt (n - 1) xs

-- RemoveAt1 :: Nat -> [k] -> [k]
-- Removes the element at the given index (1-indexed)
type family RemoveAt1 (n :: Nat) (l :: [k]) :: [k] where
    RemoveAt1 0 xs = xs
    RemoveAt1 1 (x : xs) = xs
    RemoveAt1 n (x : xs) = x : RemoveAt1 (n - 1) xs

-- CheckMember :: k -> [k] -> Constraint
-- Constraints the type to be in the list of types, erroring out if it isn't
type CheckMember (a :: k) (l :: [k]) = CheckMember' (MaybeIndexOf a l) a l :: Constraint

type family CheckMember' (i :: Nat) (a :: k) (l :: [k]) :: Constraint where
    CheckMember' 0 a l = TypeError ( Text "`"
                                :<>: ShowType a
                                :<>: Text "' is not a member of "
                                :<>: ShowType l
                                   )
    CheckMember' _ _ _ = ()

-- Member :: k -> [k] -> Constraint
-- Constrains the type to be in the list of types
type Member (x :: k) (xs :: [k]) = MemberAtIndex (IndexOf x xs) x xs :: Constraint

type MemberAtIndex (i :: Nat) (x :: k) (xs :: [k]) = (x ~ Index i xs, KnownNat i)

-- Index :: Nat -> [k] -> k
-- Returns the type at the given index
-- Type-level version of 'flip (!!)'
type Index (n :: Nat) (l :: [k]) = Index' n l l :: k

type family Index' (n :: Nat) (l :: [k]) (l2 :: [k]) :: k where
    Index' 0 (x : _) _ = x
    Index' n (_ : xs) l2 = Index' (n - 1) xs l2
    Index' n '[] l2 =
        TypeError ( Text "Index "
               :<>: ShowType n
               :<>: Text " out of bounds for list: "
               :$$: Text " "
               :<>: ShowType l2
                  )

-- MaybeIndexOf :: k -> [k] -> Nat
-- Returns the first index of the given type in the list (1-indexed), or 0 if none
type MaybeIndexOf (a :: k) (l :: [k]) = MaybeIndexOf' 0 a l :: Nat

type family MaybeIndexOf' (n :: Nat) (a :: k) (l :: [k]) :: Nat where
    MaybeIndexOf' n x '[] = 0
    MaybeIndexOf' n x (x : xs) = n + 1
    MaybeIndexOf' n x (y : xs) = MaybeIndexOf' (n + 1) x xs

-- IndexesOf :: k -> [k] -> [Nat]
-- Returns all the indices of the given type in the list (0-indexed)
type IndexesOf (a :: k) (l :: [k]) = IndexesOf' 0 a l :: [Nat]

type family IndexesOf' (n :: Nat) (a :: k) (l :: [k]) :: [Nat] where
    IndexesOf' n x '[]      = '[]
    IndexesOf' n x (x : xs) = n : IndexesOf' (n + 1) x xs
    IndexesOf' n x (y : xs) = IndexesOf' (n + 1) x xs

-- IndexOf :: k -> [k] -> Nat
-- Returns the first index of the given type in the list
-- Errors out if the type is not found
type IndexOf (x :: k) (xs :: [k]) = IndexOf' (MaybeIndexOf x xs) x xs :: Nat

type family IndexOf' (i :: Nat) (a :: k) (l :: [k]) :: Nat where
    IndexOf' 0 x l = TypeError ( ShowType x
                            :<>: Text " not found in list: "
                            :$$: Text " "
                            :<>: ShowType l
                               )
    IndexOf' i _ _ = i - 1
