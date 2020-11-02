{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-
   Module:      Type.OpenADT
   Description: Extensible recursive sum type
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This implementation is inspired by the `open-adt` package: https://github.com/woehr/open-adt
-}

module Type.OpenADT
    ( 
    -- * The recursive open-sum datatype
      OpenADT
    -- * Re-exports
    , module X
    -- * Monadic recursion-schemes
    , cataM
    , anaM
    , hyloM
    ) where

import           Data.Functor.Foldable as X (Fix (..), ana, cata, hylo, unfix)
import           Data.Functor.Foldable
import           Data.Sum              as X
import           Type.List             as X

type OpenADT r = Fix (Sum r)

-- | Monadic catamorphism
cataM :: (Recursive t, Traversable (Base t), Monad m)
      => (Base t a -> m a)
      -> t
      -> m a
cataM f = go where go = (=<<) f . traverse go . project
{-# INLINE cataM #-}

-- | Monadic anamorphism
anaM :: (Corecursive t, Traversable (Base t), Monad m)
     => (a -> m (Base t a))
     -> a
     -> m t
anaM f = go where go = fmap embed . (=<<) (traverse go) . f
{-# INLINE anaM #-}

-- | Monadic hylomorphism
hyloM :: (Monad m, Traversable f)
      => (f b -> m b)
      -> (a -> m (f a))
      -> a
      -> m b
hyloM f g = go where go = (f =<<) . (mapM go =<<) . g
{-# INLINE hyloM #-}

-- -- | 'decompose' two variants at a time
-- decompose2 :: forall a b r x. ('[a, b] :<: r, b :< Delete a r)
           -- => Sum r x
           -- -> (a :+: b :+: Sum (r \\ '[a, b])) x
-- decompose2 s =
    -- case decompose s of
      -- L1 x -> L1 x
      -- R1 s' -> R1 $
          -- case decompose s' of
            -- L1 x -> L1 x
            -- R1 s'' -> R1 s''
-- [># INLINE decompose2 #<]

-- | 'decompose' three variants at a time
-- decompose3 :: forall a b c r x. ('[a, b, c] :<: r, '[b, c] :<: Delete a r, c :< (r \\ '[a, b]))
           -- => Sum r x
           -- -> (a :+: b :+: c :+: Sum (r \\ '[a, b, c])) x
-- decompose3 s =
    -- case decompose s of
      -- L1 x -> L1 x
      -- R1 s' -> R1 $ decompose2 s'
-- [># INLINE decompose3 #<]

