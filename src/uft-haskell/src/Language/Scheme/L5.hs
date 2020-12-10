{-# OPTIONS_GHC -Wall #-}
-- Assembly codegen
module Language.Scheme.L5
    ( L5 (..)
    , L5Constant
    , L1Constant (..)
    ) where

import           Control.Monad
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Accum
import           Control.Monad.Trans.State
import           Data.Foldable (traverse_)
import           Data.DList                 (DList)
import qualified Data.DList                 as DList
import           Data.Maybe
import           Data.Text                  (Text)
-- import           Debug.Trace
import           Language.Scheme.L4         (L1Constant (..), L4)
import qualified Language.Scheme.L4         as L4
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Class
import           Language.Scheme.Primitives
import           Language.Scheme.Util

type L5Constant = L1Constant

data L5
    = DefLabel !Text
    | GotoLabel !Text
    | LoadLiteral !Int !L5Constant
    | LoadFunction !Int !Int ![L5]
    | Cmd !Prim ![Int]
    | CmdLit !Prim ![Int] !L5Constant

instance Embed L5 where
    embed = \case
        DefLabel lbl -> SList ["labal", SSymbol lbl]
        GotoLabel lbl -> SList ["goto", SSymbol lbl]
        LoadLiteral r lit -> SList ["loadliteral", reg r, embed lit]
        LoadFunction r arity instrs -> SList ["loadfunction", reg r, num arity, SList (map embed instrs)]
        Cmd p args -> SList (SSymbol (_prim_name p) : map reg args)
        CmdLit p args lit -> SList (SSymbol (_prim_name p) : map reg args ++ [embed lit])
        where
            num n = SNum (fromIntegral n)
            reg n = SSymbol $ "r" <> tshow n

instance Project L5 where
    project sexps = do
        l4 <- project sexps
        l5 <- runExcept $ traverse_ forEffect l4 `execAccumT` DList.empty `evalStateT` 0
        pure $ DList.toList l5
        where
            forEffect :: L4 -> AccumT (DList L5) (StateT Int (Except Text)) ()
            forEffect = \case
                L4.EConst _ -> pure ()
                L4.EName _ -> pure ()
                L4.EPrim p args
                  | _prim_effectful p, _prim_destreg p -> error $ "Calling prim " ++ show (_prim_name p) ++ " for effect, but it requires a dest reg"
                  | _prim_effectful p                  -> cmd p args
                  | otherwise                          -> pure ()
                L4.EPrimLit p args lit
                  | _prim_effectful p, _prim_destreg p -> error $ "Calling prim " ++ show (_prim_name p) ++ " for effect, but it requires a dest reg"
                  | _prim_effectful p                  -> cmdLit p args lit
                  | otherwise                          -> pure ()
                L4.EFuncall f args
                  | consecutive (f:args) -> cmd (prim "call") [f, f, fromMaybe f (lastMay args)]
                  | otherwise            -> error $ "Invalid funcall: not consecutive " ++ show f ++ " " ++ show args
                L4.EIf x e1 e2 -> do
                    l <- freshLabel "if-false"
                    l' <- freshLabel "if-end"
                    ifGoto x l
                    forEffect e2
                    gotoL l'
                    deflabel l
                    forEffect e1
                    deflabel l'
                L4.ELet x e1 e2 -> do
                    toReg x e1
                    forEffect e2
                L4.ESeq e1 e2 -> do
                    forEffect e1
                    forEffect e2
                L4.EWhile x e1 e2 -> do
                    l <- freshLabel "while-test"
                    l' <- freshLabel "while-body"
                    gotoL l
                    deflabel l'
                    forEffect e2
                    deflabel l
                    toReg x e1
                    ifGoto x l'
                L4.ECaptured _ -> pure ()
                L4.EClosure _ _ -> pure ()
                L4.ELocalSet x e -> toReg x e

            return_ :: L4 -> AccumT (DList L5) (StateT Int (Except Text)) ()
            return_ = \case
                L4.EName x -> cmd (prim "return") [x]
                L4.EFuncall f args
                  | consecutive (f:args) -> cmd (prim "tailcall") [f, fromMaybe f (lastMay args)]
                  | otherwise            -> error $ "Invalid funcall: not consecutive " ++ show f ++ " " ++ show args
                L4.EIf x e1 e2 -> do
                    l <- freshLabel "if-false"
                    ifGoto x l
                    return_ e2
                    deflabel l
                    return_ e1
                L4.ESeq e1 e2 -> do
                    forEffect e1
                    return_ e2
                L4.ELet x e1 e2 -> do
                    toReg x e1
                    return_ e2
                e -> do
                    toReg 0 e
                    cmd (prim "return") [0]

            toReg :: Int -> L4 -> AccumT (DList L5) (StateT Int (Except Text)) ()
            toReg r = \case
                L4.EConst k -> loadLiteral r k
                L4.EName n  -> cmd (prim "copyreg") [r, n]
                L4.EPrim p args 
                  | _prim_destreg p -> cmd p (r : args)
                  | otherwise       -> cmd p args >> loadLiteral r (KBool False)
                L4.EPrimLit p args lit 
                  | _prim_destreg p -> cmdLit p (r : args) lit
                  | otherwise       -> cmdLit p args lit >> loadLiteral r (KBool False)
                L4.EFuncall f args
                  | consecutive (f:args) -> cmd (prim "call") [r, f, fromMaybe f (lastMay args)]
                  | otherwise            -> error $ "Invalid funcall: not consecutive " ++ show f ++ " " ++ show args
                L4.EIf x e1 e2 -> do
                    l  <- freshLabel "if-false"
                    l' <- freshLabel "if-end"
                    ifGoto x l
                    toReg r e2
                    gotoL l'
                    deflabel l
                    toReg r e1
                    deflabel l'
                L4.ELet x e1 e2 -> do
                    toReg x e1
                    toReg r e2
                L4.ESeq e1 e2 -> do
                    forEffect e1
                    toReg r e2
                L4.EWhile x e1 e2 -> do
                    forEffect (L4.EWhile x e1 e2)
                    loadLiteral r (KBool False)
                L4.ECaptured n -> cmd (prim "getclslot") [r, 0, n]
                L4.EClosure (args, body) [] -> loadfunction r (length args) $ return_ body
                L4.EClosure (args, body) captured -> do
                    loadfunction r (length args) $ return_ body
                    cmd (prim "closure") [r, r, length captured]
                    forM_ (zip [0..] captured) $ \(i, x) ->
                        cmd (prim "setclslot") [r, x, i]
                L4.ELocalSet x e -> do
                    toReg r e
                    cmd (prim "copyreg") [r, x]

            loadfunction dest arity m = do
                body <- lift $ m `execAccumT` DList.empty
                add $ pure $ LoadFunction dest arity (DList.toList body)
            loadLiteral r k = add $ pure $ LoadLiteral r k
            cmd p args = add $ pure $ Cmd p args
            cmdLit p args lit = add $ pure $ CmdLit p args lit
            deflabel lbl = add $ pure $ DefLabel lbl
            ifGoto r lbl = cmd (prim "if") [r] >> gotoL lbl
            gotoL lbl = add $ pure $ GotoLabel lbl

            freshLabel :: Text -> AccumT (DList L5) (StateT Int (Except Text)) Text
            freshLabel base = do
                n <- lift get
                lift $ put (n + 1)
                pure $ base <> tshow n

            consecutive :: [Int] -> Bool
            consecutive [] = True
            consecutive [_] = True
            consecutive (x:y:ys) = x + 1 == y && consecutive (y:ys)

            lastMay [] = Nothing
            lastMay xs = Just (last xs)

