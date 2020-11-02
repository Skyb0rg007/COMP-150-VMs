
module Uft.KNormal.Codegen
    ( 
    ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Deriving
import           Data.Foldable          (foldl')
import           Data.Kind
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector
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

fresh :: MonadState Int m
      => m Text
fresh = do
    n <- get
    put $ n + 1
    pure $ "label_" <> tshow n

consec :: [Int] -> Bool
consec (x:y:xs) = x + 1 == y && consec (y:xs)
consec _ = True

toReg :: (MonadError Text m, MonadState Int m)
      => Int
      -> OpenADT (KNormal Int)
      -> m [AsmInstr]
toReg r = \case
    ExpPrimApply p args -> pure [Cmd p (Vector.cons r args)]
    ExpPrimApplyLit p args lit -> pure [CmdLit p (Vector.cons r args) lit]
    ExpLet1 x e1 e2 -> liftM2 (++) (toReg x e1) (toReg r e2)
    ExpIf (ExpGetLocal x) e1 e2 -> do
        e1' <- toReg r e1
        e2' <- toReg r e2
        lbl <- fresh
        lbl' <- fresh
        pure $
            [ Cmd (prim "if") (Vector.singleton x)
            , GotoLabel lbl
            ] ++
            e2' ++
            [ GotoLabel lbl'
            , DefLabel lbl
            ] ++
            e1' ++
            [ DefLabel lbl'
            ]
    ExpWhile {} -> error "NYI"
    ExpBegin (Vector.toList -> es) ->
        liftM2 (++)
        (foldMapM forEffect (init es))
        (toReg r (last es))
    ExpSetLocal x e ->
        fmap (++ [Cmd (prim "copyreg") (Vector.singleton x)])
        (toReg x e)
    ExpFunApply f (Vector.toList -> args) 
      | consec (f:args) ->
        pure [Cmd (prim "call") (Vector.fromList [r, f, last args])]
    LitNum n -> 
        pure [CmdLit (prim "loadliteral") Vector.empty (LitNum n)]
    LitStr n -> 
        pure [CmdLit (prim "loadliteral") Vector.empty (LitStr n)]
    LitSym n -> 
        pure [CmdLit (prim "loadliteral") Vector.empty (LitSym n)]
    LitChar n -> 
        pure [CmdLit (prim "loadliteral") Vector.empty (LitChar n)]
    LitBool n -> 
        pure [CmdLit (prim "loadliteral") Vector.empty (LitBool n)]
    LitEmpty -> 
        pure [CmdLit (prim "loadliteral") Vector.empty LitEmpty]

forEffect :: (MonadError Text m, MonadState Int m)
          => OpenADT (KNormal Int)
          -> m [AsmInstr]
forEffect = \case
    ExpPrimApply p args 
      | _prim_kind p == HasEffect -> pure [Cmd p args]
    ExpPrimApplyLit p args lit 
      | _prim_kind p == HasEffect -> pure [CmdLit p args lit]
    ExpLet1 x e1 e2 -> liftM2 (++) (toReg x e1) (forEffect e2)
    ExpIf (ExpGetLocal x) e1 e2 -> do
        e1' <- forEffect e1
        e2' <- forEffect e2
        lbl <- fresh
        lbl' <- fresh
        pure $
            [ Cmd (prim "if") (Vector.singleton x)
            , GotoLabel lbl
            ] ++
            e2' ++
            [ GotoLabel lbl'
            , DefLabel lbl
            ] ++
            e1' ++
            [ DefLabel lbl'
            ]
    ExpWhile {} -> error "NYI"
    ExpBegin (Vector.toList -> es) -> foldMapM forEffect es
    ExpFunApply f (Vector.toList -> args) 
      | consec (f:args) ->
        undefined
    ExpSetLocal x e ->
        fmap (++ [Cmd (prim "copyreg") (Vector.singleton x)])
        (toReg x e)
    _ -> pure []
