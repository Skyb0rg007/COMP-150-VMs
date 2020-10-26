{-|
   Module:      Uft.Asm.LabelElim
   Description: Label elimination pass
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   Converts 'GotoLabel lbl' into 'GotoOffset offset' and removes 'Deflabel lbl'
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Uft.Asm.LabelElim
    (
    -- * Transformation pass
      labelElim
    ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.DList           (DList)
import qualified Data.DList           as DList
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import           Uft.Asm.Ast

-- | Eliminates labels from the assembly program
labelElim :: forall m. MonadError Text m => Prog -> m Prog
labelElim instrs = do
    (_, m) <- foldM collect (0, Map.empty) instrs
    DList.toList . snd <$> foldM (replace m) (0, DList.empty) instrs
    where
        -- Collects the label positions
        collect :: (Int, Map Text Int) -> Instr -> m (Int, Map Text Int)
        collect (n, m) = \case
            Deflabel lbl
              | Just n' <- Map.lookup lbl m -> throwError ("Duplicate label " <> lbl)
              | otherwise -> pure (n, Map.insert lbl n m)
            i -> pure (succ n, m)
        -- Replaces GotoLabel with GotoOffset
        replace :: Map Text Int -> (Int, DList Instr) -> Instr -> m (Int, DList Instr)
        replace m (n, instrs) = \case
            Deflabel lbl -> pure (n, instrs)
            GotoLabel lbl
              | Just n' <- Map.lookup lbl m -> pure (succ n, instrs `DList.snoc` GotoOffset (n'-n-1))
              | otherwise -> throwError ("Undefined label " <> lbl)
            LoadFunc reg arity body -> do
                i <- LoadFunc reg arity <$> labelElim body
                pure (succ n, instrs `DList.snoc` i)
            i -> pure (succ n, instrs `DList.snoc` i)

