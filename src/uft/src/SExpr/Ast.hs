
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module SExpr.Ast
    ( SExpr (SAtom, SCons, SNil, SList, SDotted)
    ) where

import           Data.Bifunctor (first)

data SExpr atom
    = SAtom atom
    | SCons (SExpr atom) (SExpr atom)
    | SNil
    deriving (Show, Eq, Ord, Read, Functor)

pattern SList :: [SExpr atom] -> SExpr atom
pattern SList xs <- (toSList -> Just xs)
    where SList xs = fromSList xs

pattern SDotted :: [SExpr atom] -> atom -> SExpr atom
pattern SDotted xs x <- (toSDotted -> Just (xs, x))
    where SDotted xs x = fromSDotted xs x

{-# COMPLETE SAtom, SDotted, SList #-}

toSList :: SExpr atom -> Maybe [SExpr atom]
toSList = \case
    SAtom _    -> Nothing
    SNil       -> Just []
    SCons x xs -> (x :) <$> toSList xs

fromSList :: [SExpr atom] -> SExpr atom
fromSList = foldr SCons SNil

toSDotted :: SExpr atom -> Maybe ([SExpr atom], atom)
toSDotted SAtom{} = Nothing
toSDotted sexp = go sexp
    where go :: SExpr atom -> Maybe ([SExpr atom], atom)
          go = \case
            SAtom atom -> Just ([], atom)
            SNil       -> Nothing
            SCons x xs -> first (x :) <$> go xs

fromSDotted :: [SExpr atom] -> atom -> SExpr atom
fromSDotted xs x = foldr SCons (SAtom x) xs
