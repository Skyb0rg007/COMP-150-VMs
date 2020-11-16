
module Uft.Asm.ToVO
    ( compileObjProg
    ) where

import           Data.Char              (ord)
import           Data.Kind              (Type)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as Lazy (Text)
import qualified Data.Text.Lazy         as Lazy.Text
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector
import           Data.Void              (Void)
import           Type.OpenADT
import           Uft.Asm.Ast
import           Uft.Pretty
import           Uft.Primitives
import           Uft.Util

bshow :: Show a => a -> Builder
bshow = B.fromString . show

compileLiteral :: AsmLit -> Builder
compileLiteral = \case
    LitNum n -> bshow n
    LitStr s -> "string " <>
        bshow (Text.length s) <> " " <>
        B.fromText (Text.intercalate " " (map (tshow . ord) (Text.unpack s)))
    LitSym s      -> compileLiteral (LitStr s)
    LitChar c     -> error "Compiling characters is NYI"
    LitBool True  -> "true"
    LitBool False -> "false"
    LitEmpty      -> "emptylist"

compileInstr :: ObjInstr -> Builder
compileInstr = \case
    GotoOffset n -> "goto " <> bshow n
    LoadLiteral r l -> "loadliteral " <> bshow r <> compileLiteral l
    LoadFunction r arity body ->
           ".load " <> bshow r <> " function " <> bshow arity <> " " <> bshow (length body + 1)
        <> mconcat (map ((<> "\n") . compileInstr) body)
        <> "\nhalt"
    Cmd p args ->
        B.fromText (_prim_name p) <>
        Vector.foldl' (\acc arg -> acc <> " " <> bshow arg) "" args
    CmdLit p args lit ->
        B.fromText (_prim_name p) <>
        Vector.foldl' (\acc arg -> acc <> " " <> bshow arg) "" args <>
        " " <> compileLiteral lit

compileProg :: [ObjInstr] -> Builder
compileProg prog =
       ".load module " <> bshow (length prog)
    <> mconcat (map ((<> "\n") . compileInstr) prog)

compileObjProg :: [ObjInstr] -> Text
compileObjProg = Lazy.Text.toStrict . B.toLazyText . compileProg

