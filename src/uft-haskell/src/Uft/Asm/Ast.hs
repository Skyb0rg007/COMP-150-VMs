
module Uft.Asm.Ast
    ( module Uft.Asm.Ast
    ) where

import           Data.Kind             (Type)
import           Data.Text             (Text)
import           Data.Vector           (Vector)
import           Data.Void             (Void)
import           Type.OpenADT
import           Uft.Pretty
import           Uft.Primitives
import           Uft.Scheme.Ast
import           Uft.Scheme.ListExpand

-- | Final literal type
type Literal = OpenADT
    '[ LitNumF
     , LitStrF
     , LitSymF
     , LitCharF
     , LitBoolF
     , LitEmptyF
     ]

-- | Trees that shrink
type family XInstr x :: Type

-- | Instruction datatype
data InstrX x
    = DefLabelX !(XInstr x) !Text
    | GotoLabelX !(XInstr x) !Text
    | GotoOffset !Int
    | LoadLiteral !Int !Literal
    | LoadFunction !Int !Int ![InstrX x]
    | Cmd !Prim !(Vector Int)
    | CmdLit !Prim !(Vector Int) !Literal

-- * Assembly
data Asm
type instance XInstr Asm = ()
type AsmInstr = InstrX Asm

-- * Object code
data Obj
type instance XInstr Obj = Void
type ObjInstr = InstrX Obj

-- Note: don't use these patterns in 'case' expressions
-- The exhaustive checking doesn't work

pattern DefLabel :: Text -> AsmInstr
pattern DefLabel x = DefLabelX () x

pattern GotoLabel :: Text -> AsmInstr
pattern GotoLabel x = GotoLabelX () x

