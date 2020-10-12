
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Uft.Lambda.ClosureConvert
    ( 
    ) where

import           Uft.Lambda.Ast
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Map.Strict (Map)
import           Control.Monad.State.Strict


data Env = Env
    { _env_closures       :: [Def]
    , _env_currentDefName :: Text
    }

setDefName :: MonadState Env m => Text -> m ()
setDefName name = modify' $ \s -> s { _env_currentDefName = name }

addDef :: MonadState Env m => Def -> m ()
addDef def = modify' $ \s -> s { _env_closures = def : _env_closures s }

liftLam :: forall m. MonadState Env m => Program -> m Exp
liftLam = undefined

