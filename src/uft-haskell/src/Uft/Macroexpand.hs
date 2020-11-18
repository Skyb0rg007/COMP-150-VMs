
module Uft.Macroexpand
    ( 
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Uft.SExpr.Types
import           Uft.Naming
import           Control.Monad.State.Strict

type Env = HashMap Name Denotation

data Denotation
    = Special String -- Builtin forms like lambda and let
    | Macro [(SExpr, SExpr)] Env
    | Identifier Name

-- Environment lookup
macroLookup :: Env -> Name -> Denotation
macroLookup env name = 
    case HashMap.lookup name env of
      Nothing -> Identifier name
      Just d  -> d

-- Environment extension
macroBind :: Env -> Name -> Denotation -> Env
macroBind env name d = HashMap.insert name d env

-- Union, prioritizing the second env
macroDivert :: Env -> Env -> Env
macroDivert env1 env2 = HashMap.union env2 env1

isVar (SAtom (SSymbol x)) = Just x
isVar _ = Nothing

-- macroExpand
    -- :: (MonadState Int m)
    -- => Env
    -- -> SExpr
    -- -> m SExpr
-- macroExpand env = \case
    -- SAtom (SSymbol name)
      -- | Identifier x <- macroLookup env name -> pure $ SAtom (SSymbol x)
    -- SList (k0 : xs : body)
      -- | Special "lambda" <- macroLookup env k0
      -- , Just xs' <- traverse isVar xs
      -- -> 

