
module Uft.KNormal.RegisterAlloc
    ( 
    ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Deriving
import           Data.Foldable          (foldl')
import           Data.Sum
import           Data.Kind
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import qualified Data.Vector            as V
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Primitives
import           Uft.Scheme.Ast
import           Uft.Scheme.ConvertPrim
import           Uft.Scheme.Disambiguate
import           Uft.Scheme.ListExpand
import           Uft.Util
import           Uft.KNormal.FromUnamb
import           Uft.Asm.Ast

newtype RegSet = RS Int

smallest :: RegSet -> Int
smallest (RS n) = n

(-:-) :: RegSet -> Int -> RegSet
(-:-) (RS n) r = RS (max n (r + 1))

bindAnyReg :: forall r. ('[ExpKLetF Int, ExpGetLocalF Int] :<: r)
    => RegSet
    -> OpenADT r
    -> (Int -> OpenADT r)
    -> OpenADT r
bindAnyReg a (ExpGetLocal n) k = k n
bindAnyReg a e k =
    let r = smallest a
     in ExpKLet r e (k r)

