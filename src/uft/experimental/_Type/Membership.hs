{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Type.Membership
    ( 
    ) where

import           Data.Text (Text)
import           Data.Void (Void)
import           Data.Kind (Type)
import           Data.Typeable
import           Data.Proxy (Proxy (..))
import           GHC.TypeLits

newtype Membership (xs :: [k]) (x :: k) = Membership { membershipId :: Int }
    deriving (Typeable)

instance Show (Membership xs x) where
    show (Membership n) = "$(mkMembership " ++ show n ++ ")"

instance Eq (Membership xs x) where
    _ == _ = True

instance Ord (Membership xs x) where
    compare _ _ = EQ

instance Semigroup (Membership xs x) where
    x <> _ = x

--
class Member xs x where
    membership :: Membership xs x

instance (Elaborate x (FindType x xs) ~ Expecting pos, KnownNat pos)
  => Member xs x where
    membership = Membership (fromInteger $ natVal (Proxy :: Proxy pos))

data Elaborated k v = Expecting v | Missing k | Duplicate k

type family Elaborate (key :: k) (xs :: [v]) :: Elaborated k v where
    Elaborate k '[]  = Missing k
    Elaborate k '[x] = Expecting x
    Elaborate k xs   = Duplicate k

type family FindType (x :: k) (xs :: [k]) :: [Nat] where
    FindType x (x : xs) = 0 : MapSucc (FindType x xs)
    FindType x (y : ys) = MapSucc (FindType x ys)
    FindType x '[] = '[]

type family MapSucc (xs :: [Nat]) :: [Nat] where
    MapSucc '[] = '[]
    MapSucc (x : xs) = 1 + x : MapSucc xs



