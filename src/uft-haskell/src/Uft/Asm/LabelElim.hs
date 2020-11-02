
-- {-# OPTIONS_GHC -Wall #-}

module Uft.Asm.LabelElim
    ( 
    ) where

import           Uft.Asm.Ast
import           Data.Text
import           Control.Monad.Except
import           Control.Monad.State
import           Data.DList           (DList)
import qualified Data.DList           as DList
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import           Uft.Asm.Ast

-- | Eliminates labels from the assembly program
labelElim :: forall m. (MonadError Text m)
          => [AsmInstr]
          -> m [ObjInstr]
labelElim instrs = do
    (_, m) <- foldM collect (0, Map.empty) instrs
    DList.toList . snd <$> foldM (replace m) (0, DList.empty) instrs
    where
        -- Collects the label positions
        collect :: (Int, Map Text Int) -> AsmInstr -> m (Int, Map Text Int)
        collect (n, m) = \case
            DefLabel lbl
              | Just n' <- Map.lookup lbl m -> throwError ("Duplicate label " <> lbl)
              | otherwise -> pure (n, Map.insert lbl n m)
            i -> pure (succ n, m)
        -- Replaces GotoLabel with GotoOffset
        replace :: Map Text Int -> (Int, DList ObjInstr) -> AsmInstr -> m (Int, DList ObjInstr)
        replace m (n, instrs) = \case
            DefLabelX () lbl -> pure (n, instrs)
            GotoLabelX () lbl
              | Just n' <- Map.lookup lbl m -> pure (succ n, instrs `DList.snoc` GotoOffset (n'-n-1))
              | otherwise -> throwError ("Undefined label " <> lbl)
            GotoOffset n -> pure (succ n, instrs `DList.snoc` GotoOffset n)
            LoadLiteral r l -> pure (succ n, instrs `DList.snoc` LoadLiteral r l)
            LoadFunction reg arity body -> do
                i <- LoadFunction reg arity <$> labelElim body
                pure (succ n, instrs `DList.snoc` i)
            Cmd p args -> pure (succ n, instrs `DList.snoc` Cmd p args)
            CmdLit p args l -> pure (succ n, instrs `DList.snoc` CmdLit p args l)

