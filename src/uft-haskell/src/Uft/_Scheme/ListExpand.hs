{-# LANGUAGE TemplateHaskell #-}
{-
   Module:      Uft.Scheme.ListExpand
   Description: Convert list literals into pairs and empty list
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.Scheme.ListExpand
    ( listExpand
    , LitEmptyF (LitEmptyF')
    , pattern LitEmptyF
    , pattern LitEmpty
    , LitPairF (LitPairF')
    , pattern LitPairF
    , pattern LitPair
    ) where

import           Data.Deriving
import           Data.Foldable   (foldl')
import           Data.Kind
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Scheme.Ast
import           Uft.Util

-- | Empty-list
data LitEmptyF (a :: Type) = LitEmptyF'
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Pair
data LitPairF a = LitPairF' !a !a
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

derive [deriveOpenADT, deriveEq1, deriveOrd1, deriveShow1, deriveRead1]
    [''LitEmptyF, ''LitPairF]

instance PrettyF LitEmptyF where
    prettyF' LitEmptyF' = "()"
instance PrettyF LitPairF where
    prettyF' (LitPairF' a b) = "(" <> a <+> "." <+> b <> ")"

listExpand
    :: forall r new old.
        ( old ~ '[LitListF, LitDotListF, LitUnquoteF, LitUnquoteSplicingF]
        , new ~ '[LitEmptyF, LitPairF]
        , old :<: r
        , Apply Functor r
        )
    => OpenADT r
    -> OpenADT (new ++ (r \\ old))
listExpand = cata alg where
    alg :: Sum r (OpenADT (new ++ (r \\ old)))
        -> OpenADT (new ++ (r \\ old))
    alg x =
        case decompose4 x of
          L1 (LitListF' xs)                         -> foldr LitPair LitEmpty xs
          R1 (L1 (LitDotListF' xs x))               -> foldr LitPair x xs
          R1 (R1 (L1 LitUnquoteF' {}))              -> undefined
          R1 (R1 (R1 (L1 LitUnquoteSplicingF' {}))) -> undefined
          R1 (R1 (R1 (R1 x')))                      -> Fix $ weaken2 x'

