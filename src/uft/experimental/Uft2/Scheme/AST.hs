
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Uft2.Scheme.AST
    ( 
    ) where

import           Data.Fix   (Fix (..))
import           Data.Kind  (Type)
import           Data.Text  (Text)
import           Data.Void  (Void, absurd)
import           Uft2.DTALC

-- | Literals
type Literal = LiteralF (Fix LiteralF)

type LiteralF = Summed '[LitIntF, LitSymF, LitBoolF, LitEmptyF, LitPairF]

newtype LitIntF a = LitIntF Int
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype LitSymF a = LitSymF Text
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

newtype LitBoolF a = LitBoolF Bool
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data LitPairF a = LitPairF !a !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data LitEmptyF a = LitEmptyF
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Expressions
type Exp = Summed '[ExpLit]

data ExpLit a = ExpLit !Literal
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


class Functor f => ToString f where
    toString :: f String -> String

instance ToString LitIntF where
    toString (LitIntF n) = show n

instance (ToString f, ToString (Summed fs), Functor (Summed (f ': fs)))
  => ToString (Summed (f ': fs)) where
    toString (Here x) = toString x
    toString (There x) = toString x

instance ToString (Summed '[]) where
    toString (SummedNil void) = absurd void

-- instance All ToString fs => ToString (Summable fs) where
    -- toString = undefined

