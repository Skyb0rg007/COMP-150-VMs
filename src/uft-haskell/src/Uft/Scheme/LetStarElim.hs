{-
   Module:      Uft.Scheme.LetStarElim
   Description: Convert "let*" into a series of "let"
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.Scheme.LetStarElim
    ( letStarElim
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Deriving
import           Data.Foldable        (foldl')
import           Data.Functor         ((<&>))
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.Kind
import           Data.Text            (Text)
import qualified Data.Vector          as Vector
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Scheme.Ast
import           Uft.Util

letStarElim
    :: forall r.
        ( ExpLetStarF :< r
        , ExpLetF :< Delete ExpLetStarF r
        , Apply Functor r
        )
   => OpenADT r
   -> OpenADT (Delete ExpLetStarF r)
letStarElim = cata alg where
    alg :: Sum r (OpenADT (Delete ExpLetStarF r))
        -> OpenADT (Delete ExpLetStarF r)
    alg x =
        case decompose x of
          L1 (ExpLetStarF' binds body) ->
              let f e b = ExpLet (Vector.singleton b) e
               in foldl f body binds
          R1 x' -> Fix x'

