{-# LANGUAGE AllowAmbiguousTypes #-}
{-
   Module:      Uft.Transform
   Description: Typeclass for AST transformations
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.Transform
    ( Transform (..)
    , transform
    ) where

import           Control.Monad.Except
import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text)
import           GHC.TypeLits         (Symbol)

class Transform (name :: Symbol) where
    type From name
    type To name
    transform' :: MonadError Text m => proxy name -> From name -> m (To name)

transform
    :: forall name from to m. (MonadError Text m, Transform name)
    => From name
    -> m (To name)
transform = transform' (Proxy :: Proxy name)

