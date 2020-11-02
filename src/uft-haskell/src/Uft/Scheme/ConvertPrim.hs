{-# LANGUAGE TemplateHaskell #-}
{-
   Module:      Uft.Scheme.ConvertPrim
   Description: Convert primitives to their own datatype
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.Scheme.ConvertPrim
    ( convertPrim
    , ExpPrimF (ExpPrimF')
    , pattern ExpPrim
    , pattern ExpPrimF
    ) where

import           Data.Deriving
import           Data.Foldable   (foldl')
import           Data.Kind
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Scheme.Ast
import           Uft.Util
import           Uft.Primitives

newtype ExpPrimF (a :: Type) = ExpPrimF' Prim
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

derive [deriveOpenADT, deriveEq1, deriveOrd1, deriveShow1, deriveRead1]
    [''ExpPrimF]

convertPrim
    :: forall r.
        ( '[ExpPrimF, ExpVarF] :<: r
        , Apply Functor r
        )
   => OpenADT r
   -> OpenADT r
convertPrim = cata alg where
    alg :: Sum r (OpenADT r) -> OpenADT r
    alg (ExpVarF x)
      | Just p <- parsePrim x = ExpPrim p
    alg x = Fix x

