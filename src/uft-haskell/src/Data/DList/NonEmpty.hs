
module Data.DList.NonEmpty
    ( 
    ) where

import           Prelude hiding (foldr, foldr1, head, tail)
import qualified Prelude
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

newtype NEDList a = NEDList { unNEDList :: [a] -> [a] }

fromList :: NonEmpty a -> NEDList a
fromList = NEDList . (++) . NonEmpty.toList

toList :: NEDList a -> NonEmpty a
toList = NonEmpty.fromList . ($ []) . unNEDList

apply :: NEDList a -> [a] -> NonEmpty a
apply l = NonEmpty.fromList . unNEDList l

singleton :: a -> NEDList a
singleton = NEDList . (:)

cons :: a -> NEDList a -> NEDList a
cons x xs = NEDList $ (x :) . unNEDList xs

snoc :: NEDList a -> a -> NEDList a
snoc xs x = NEDList $ unNEDList xs . (x :)

append :: NEDList a -> NEDList a -> NEDList a
append (NEDList xs) (NEDList ys) = NEDList (xs . ys)

concat :: NonEmpty (NEDList a) -> NEDList a
concat = Prelude.foldr1 append

head :: NEDList a -> a
head xs = NonEmpty.head (toList xs)

tail :: NEDList a -> [a]
tail xs = NonEmpty.tail (toList xs)

unfoldr :: (a -> (b, Maybe a)) -> a -> NEDList b
unfoldr f z =
    case f z of
      (x, Nothing) -> singleton x
      (x, Just z') -> cons x (unfoldr f z')

foldr :: (a -> b -> b) -> b -> NEDList a -> b
foldr f z = Prelude.foldr f z . toList

foldr1 :: (a -> a -> a) -> NEDList a -> a
foldr1 f = Prelude.foldr1 f . toList

map :: (a -> b) -> NEDList a -> NEDList b
map f = fromList . NonEmpty.map f . toList

