{-# LANGUAGE StrictData #-}

module Language.Scheme.Asm.Ast
    ( module Language.Scheme.Asm.Ast
    ) where

import           Data.Kind             (Type)
import           Data.Text             (Text)
import           Data.Vector           (Vector)
import           Data.Void             (Void)
import           Uft.Primitives

data AsmLit
    = LitStr Text
    | LitSym Text
    | LitChar Char
    | LitNum Double
    | LitEmpty
    | LitBool Bool
    deriving (Show, Eq, Ord, Read)

-- | Trees that shrink
type family XInstr x :: Type

-- | Instruction datatype
data InstrX x
    = DefLabelX !(XInstr x) !Text
    | GotoLabelX !(XInstr x) !Text
    | GotoOffset !Int
    | LoadLiteral !Int !AsmLit
    | LoadFunction !Int !Int ![InstrX x]
    | Cmd !Prim !(Vector Int)
    | CmdLit !Prim !(Vector Int) !AsmLit

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

