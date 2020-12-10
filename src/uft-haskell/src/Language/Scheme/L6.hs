{-# OPTIONS_GHC -Wall #-}
-- Label elimination + object code generation
module Language.Scheme.L6
    ( L6 (..)
    , L6Constant
    , L1Constant (..)
    , compileObjProg
    ) where

import           Control.Monad
import           Data.Char                  (ord)
import           Data.DList                 (DList)
import qualified Data.DList                 as DList
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as Lazy.Text
import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Text.Lazy.Builder     as B
-- import           Debug.Trace
import           Language.Scheme.L5         (L1Constant (..), L5)
import qualified Language.Scheme.L5         as L5
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Class
import           Language.Scheme.Primitives
import           Language.Scheme.Util

type L6Constant = L1Constant

data L6
    = GotoOffset !Int
    | LoadLiteral !Int !L6Constant
    | LoadFunction !Int !Int ![L6]
    | Cmd !Prim ![Int]
    | CmdLit !Prim ![Int] !L6Constant

instance Embed L6 where
    embed = \case
        GotoOffset n -> SList ["goto", num n]
        LoadLiteral r lit -> SList ["loadliteral", reg r, embed lit]
        LoadFunction r arity instrs -> SList ["loadfunction", reg r, num arity, SList (map embed instrs)]
        Cmd p args -> SList (SSymbol (_prim_name p) : map reg args)
        CmdLit p args lit -> SList (SSymbol (_prim_name p) : map reg args ++ [embed lit])
        where
            num n = SNum (fromIntegral n)
            reg n = SSymbol $ "r" <> tshow n

instance Project L6 where
    project = labelElim <=< project @L5 where
        labelElim :: [L5] -> Either Text [L6]
        labelElim l5 = do
            (_, m) <- foldM collect (0, HashMap.empty) l5
            DList.toList . snd <$> foldM (replace m) (0, DList.empty) l5

        collect
            :: (Int, HashMap Text Int)
            -> L5
            -> Either Text (Int, HashMap Text Int)
        collect (n, m) = \case
            L5.DefLabel lbl
              | Just _ <- HashMap.lookup lbl m -> Left $ "Duplicate label " <> lbl
              | otherwise -> pure (n, HashMap.insert lbl n m)
            _ -> pure (succ n, m)

        replace
            :: HashMap Text Int
            -> (Int, DList L6)
            -> L5
            -> Either Text (Int, DList L6)
        replace m (n, instrs) = \case
            L5.DefLabel _ -> pure (n, instrs)
            L5.GotoLabel lbl
              | Just n' <- HashMap.lookup lbl m -> pure (succ n, instrs `DList.snoc` GotoOffset (n' - n - 1))
              | otherwise -> Left $ "Undefined label " <> lbl
            L5.LoadLiteral r l -> pure (succ n, instrs `DList.snoc` LoadLiteral r l)
            L5.LoadFunction reg arity body -> do
                body' <- labelElim body
                pure (succ n, instrs `DList.snoc` LoadFunction reg arity body')
            L5.Cmd p args -> pure (succ n, instrs `DList.snoc` Cmd p args)
            L5.CmdLit p args lit -> pure (succ n, instrs `DList.snoc` CmdLit p args lit)

--

bshow :: Show a => a -> Builder
bshow = B.fromString . show

compileLiteral :: L6Constant -> Builder
compileLiteral = \case
    KNum n -> bshow n
    KString s -> "string " <>
        bshow (Text.length s) <> " " <>
        B.fromText (Text.intercalate " " (map (tshow . ord) (Text.unpack s)))
    KSymbol s      -> compileLiteral (KString s)
    KChar c     -> bshow (ord c)
    KBool True  -> "true"
    KBool False -> "false"
    KEmpty      -> "emptylist"

compileInstr :: L6 -> Builder
compileInstr = \case
    GotoOffset n -> "goto " <> bshow n
    LoadLiteral r l -> "loadliteral " <> bshow r <> " " <> compileLiteral l
    LoadFunction r arity body ->
           ".load function " <> bshow r <> " " <> bshow arity <> " " <> bshow (length body + 1)
        <> "\n"
        <> mconcat (map ((<> "\n") . compileInstr) body)
        <> "halt"
    Cmd p args ->
        B.fromText (_prim_voName p) <>
        foldl (\acc arg -> acc <> " " <> bshow arg) "" args
    CmdLit p args lit ->
        B.fromText (_prim_voName p) <>
        foldl (\acc arg -> acc <> " " <> bshow arg) "" args <>
        " " <> compileLiteral lit

compileProg :: [L6] -> Builder
compileProg prog =
       ".load module " <> bshow (length prog) <> "\n"
    <> mconcat (map ((<> "\n") . compileInstr) prog)

compileObjProg :: [L6] -> Text
compileObjProg = Lazy.Text.toStrict . B.toLazyText . compileProg
