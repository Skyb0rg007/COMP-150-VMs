{-# LANGUAGE UndecidableInstances #-}
{-
   Module:      Type.List
   Description: Type-level functions operating on type-lists
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   Function naming + implementation comes from 'Prelude'
-}

module Type.List
    ( 
    -- * Lifted prelude functions
      type (++)
    , type (\\)
    , Delete
    , Nub
    , Union
    , Length
    ) where

import           Data.Kind    (Type)
import           GHC.TypeLits (type (+), Nat)

-- | Append two type-lists
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    (++) (x : xs) ys = x : (xs ++ ys)
    (++) '[] ys = ys

-- | Remove all occurrences of a type from a type-list
-- This differs from 'delete' since 'delete' only removes the first occurrence
type family Delete (x :: k) (xs :: [k]) where
    Delete x (x : xs) = Delete x xs
    Delete y (x : xs) = x : Delete y xs
    Delete x '[]      = '[]

-- | Filter out the given types from a type-list
-- This differs from 'Prelude.\\' since 'Prelude.\\' only removes the first occurrence
type family (\\) (xs :: [k]) (ys :: [k]) :: [k] where
    (\\) xs (y : ys) = (\\) (Delete y xs) ys
    (\\) xs '[] = xs

-- | Remove all duplicate types from a type-list
type family Nub (xs :: [k]) :: [k] where
    Nub '[]      = '[]
    Nub (x : xs) = x : Nub (Delete x xs)

-- | Append two type-lists, preventing the append from creating duplicates
type family Union (xs :: [k]) (ys :: [k]) :: [k] where
    Union xs '[] = xs
    Union xs ys = xs ++ (Nub ys \\ xs)

-- | List length
type family Length (xs :: [k]) :: Nat where
    Length '[] = 0
    Length (_ : xs) = 1 + Length xs
