
-- TODO: Not yet ready
module Language.Scheme.L0.MacroExpand
    ( 
    ) where

import           Data.Text (Text)
import           Language.Scheme.L0.Ast
import           Type.OpenADT
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.Maybe
import           Data.Functor ((<&>))

-- 

data VBind
    = VMacro (Env -> [OpenADT L0] -> OpenADT L0)
    | VKeyword Text
    | VGlobal Text

type Env = HashMap Text VBind

data Pat
    = PKeyword Env (OpenADT L0)
    | PVar (OpenADT L0)
    | PValue (OpenADT L0)
    | PPair Pat Pat
    | PVector [Pat]
    | PMulti Pat Pat
    | PVMulti Pat
    | PMVector [Pat]

data PatVar
    = VItem (OpenADT L0)
    | VMulti [PatVar]

