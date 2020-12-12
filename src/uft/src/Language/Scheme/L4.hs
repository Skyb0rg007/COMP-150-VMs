{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}
-- Register allocation and k-normalization
module Language.Scheme.L4
    ( L4 (..)
    , L4Constant
    , L1Constant (..)
    ) where

import           Control.Monad
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Unique
-- import           Debug.Trace
import           Language.Scheme.L3         (L1Constant (..), L3)
import qualified Language.Scheme.L3         as L3
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Class
import           Language.Scheme.Primitives
import           Language.Scheme.Util

type L4Constant = L1Constant

data L4
    = EConst L4Constant
    | EName Int
    | EPrim Prim [Int]
    | EPrimLit Prim [Int] L4Constant
    | EFuncall Int [Int]
    | EIf Int L4 L4
    | ELet Int L4 L4
    | ESeq L4 L4
    | EWhile Int L4 L4
    | ECaptured Int
    | EClosure ([Int], L4) [Int]
    | ELocalSet Int L4

instance Project L4 where
    project = traverse (knormalize HashMap.empty (RS 0)) <=< project

instance Embed L4 where
    embed = \case
        EConst k                -> embed k
        EName n                 -> name n
        EPrim p args            -> SList (SSymbol (_prim_name p) : map name args)
        EPrimLit p args lit     -> SList (SSymbol (_prim_name p) : map name args ++ [embed lit])
        EFuncall f args         -> SList (name f : map name args)
        EIf a b c               -> SList ["if", name a, embed b, embed c]
        ELet x a b              -> SList ["let", name x, embed a, embed b]
        ESeq a b                -> SList ["seq", embed a, embed b]
        EWhile x a b            -> SList ["while", SList ["let", SList [SList [name x, embed a]]], embed b]
        ECaptured n             -> SList ["getclslot", SNum (fromIntegral n)]
        EClosure (args, body) c -> SList ("closure" : SList ["lambda", SList (map name args), embed body] : map name c)
        ELocalSet x e           -> SList ["set!", name x, embed e]
        where
            name n = SSymbol $ "r" <> tshow n

-- * Helpers

newtype RegSet = RS Int

smallest :: RegSet -> Int
smallest (RS n) = n

(-:-) :: RegSet -> Int -> RegSet
(-:-) (RS n) r = RS (max n r + 1)

bindAnyReg
    :: Monad m
    => RegSet
    -> L4
    -> (Int -> m L4)
    -> m L4
bindAnyReg _ (EName n) k = k n
bindAnyReg a e k = bindSmallest a e k

bindSmallest
    :: Monad m
    => RegSet
    -> L4
    -> (Int -> m L4)
    -> m L4
bindSmallest a e k =
    let r = smallest a
     in elet r e <$> k r

elet :: Int -> L4 -> L4 -> L4
elet y (ELet x e1 e2) e3 = ELet x e1 (ELet y e2 e3)
elet x e1 e2 = ELet x e1 e2

nbRegsWith
    :: Monad m
    => (RegSet -> a -> m L4)
    -> (RegSet -> L4 -> (Int -> m L4) -> m L4)
    -> RegSet
    -> [a]
    -> ([Int] -> m L4)
    -> m L4
nbRegsWith normalize p a0 xs k = go a0 xs where
    go _ [] = k []
    go a (e:es) = do
        e' <- normalize a e
        p a e' $ \r -> nbRegsWith normalize p (a -:- r) es (k . (r:))

knormalize
    :: Monad m
    => HashMap Unique Int
    -> RegSet
    -> L3
    -> m L4
knormalize rho a = \case
    L3.EConst k -> pure $ EConst k
    L3.EPrimApply p args ->
        nbRegs bindAnyReg a args $ \args' ->
            pure $ EPrim p args'
    L3.EApply f args -> do
        f' <- knormalize rho a f
        bindSmallest a f' $ \r ->
            nbRegs bindSmallest (a -:- r) args $ \args' ->
                pure $ EFuncall r args'
    L3.ELocalSet x e ->
        let r = rho HashMap.! x
         in ELocalSet r <$> knormalize rho a e
    L3.EGlobalVar x ->
        pure $ EPrimLit (prim "get-global") [] (KSymbol x)
    L3.EGlobalSet x e -> do
        e' <- knormalize rho a e
        bindAnyReg a e' $ \r ->
            pure $ EPrimLit (prim "set-global!") [r] (KSymbol x)
    L3.EBegin [] -> pure $ EPrim (prim "void") []
    L3.EBegin [e] -> knormalize rho a e
    L3.EBegin (e:es) -> ESeq <$> knormalize rho a e <*> knormalize rho a (L3.EBegin es)
    L3.EIf e1 e2 e3 -> do
        e1' <- knormalize rho a e1
        e2' <- knormalize rho a e2
        e3' <- knormalize rho a e3
        bindAnyReg a e1' $ \r ->
            pure $ EIf r e2' e3'
    L3.EWhile e1 e2 -> do
        let r = smallest a
        e1' <- knormalize rho a e1
        e2' <- knormalize rho a e2
        pure $ EWhile r e1' e2'
    L3.ELet (unzip -> (names, es)) body ->
        nbRegs bindAnyReg a es $ \rs ->
            let rho' = HashMap.fromList (zip names rs) <> rho
             in knormalize rho' (removeRegisters a rs) body
    L3.EClosure (args, body) closed ->
        nbRegs bindAnyReg a closed $ \rs ->
            EClosure <$> funcode (args, body) (rho, RS 1) <*> pure rs
    L3.EClosedVar n -> pure $ ECaptured n
    L3.ELocalVar x -> 
        case HashMap.lookup x rho of
          Nothing -> error $ "Missing #" ++ show (hashUnique x)
          Just n -> pure (EName n)
    where
        removeRegisters rs [] = rs
        removeRegisters rs (x:xs) = removeRegisters (rs -:- x) xs
        nbRegs = nbRegsWith (knormalize rho)
        funcode (args, body) (rho, rs) =
            let f (bindings, a) x = (HashMap.insert x (smallest a) bindings, a -:- smallest a)
                (rho', rs') = foldl f (rho, rs) args
                consec = (rho' HashMap.!) <$> args
             in (consec,) <$> knormalize rho' rs' body

