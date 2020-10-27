
module Uft.Asm.Ast
    ( module Uft.Asm.Ast
    ) where

import           Control.Monad.Except
import           Data.Foldable        (traverse_)
import           Data.Maybe           (isJust)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector
import           Uft.Primitives
import           Uft.AstNodes
import           Data.Row
import           Type.OpenADT

type Prog = [Instr]

type LitRowF =
    ( "num"   .== LitNumF
   .+ "str"   .== LitStrF
   .+ "bool"  .== LitBoolF
   .+ "empty" .== LitEmptyF
   .+ "nil"   .== LitNilF
    )
type Lit = OpenADT LitRowF

-- | A single Uft assembly instruction
data Instr
    = InstrDeflabel !Text              -- lbl:
    | InstrGotoLabel !Text             -- goto lbl
    | InstrGotoOffset !Int             -- goto n
    | InstrLoadLit !Int !Lit           -- %1 := <lit>
    | InstrLoadFunc !Int !Int ![Instr] -- %1 := function n { <instr>* }
    | InstrPrim !(SomePrim Int Int)
    deriving (Show, Eq, Ord)


