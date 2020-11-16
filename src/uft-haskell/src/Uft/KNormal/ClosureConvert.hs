
module Uft.KNormal.ClosureConvert
    ( module Uft.KNormal.ClosureConvert
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable        (fold)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.List            ((\\))
import           Data.Text            (Text)
import           Type.OpenADT
import           Uft.KNormal.Types
import           Uft.UScheme.Types
import           Uft.Util

type ClosedScheme = UScheme ++ '[KNCapturedF, KNClosureF Text, KNLetRecF Text]

-- | Get the free variables in an expression
freeVarAlg :: Sum UScheme (HashSet Text) -> HashSet Text
freeVarAlg = \case
    ESetLocalF x e -> HashSet.insert x e
    ELetF (unzip -> (names, es)) e ->
         HashSet.unions es <> (e `HashSet.difference` HashSet.fromList names)
    ELetRecF (unzip -> (names, es)) e ->
         (HashSet.unions es <> e) `HashSet.difference` HashSet.fromList names
    EVarLocalF x -> HashSet.singleton x
    ELambdaF args e ->
        e `HashSet.difference` HashSet.fromList args
    x -> fold x

-- | closeExp is a great example of a zygomorphism
-- The freeVarAlg F-algebra returns the free variables in a given subexpression
-- Those free variables are used in the alg F-algebra to close the expression
closeExp
    :: forall m. (MonadError Text m, MonadReader (HashMap Text Int) m)
    => OpenADT UScheme
    -> m (OpenADT ClosedScheme)
closeExp = zygo freeVarAlg alg where
    alg :: Sum UScheme (HashSet Text, m (OpenADT ClosedScheme))
        -> m (OpenADT ClosedScheme)
    alg adt = do
        captured <- ask
        case adt of
          EVarLocalF x 
            | Just n <- HashMap.lookup x captured -> pure $ KNCaptured n
          ESetLocalF x _
            | Just n <- HashMap.lookup x captured ->
                throwError $ "Attempt to write to captured variable " <> x
          ELambdaF args (freeBody, body) -> do
              let closedNames :: [Text]
                  closedNames = HashSet.toList freeBody \\ args
                  closedNames' :: HashMap Text Int
                  closedNames' = HashMap.fromList $ zip closedNames [0..]
              body' <- local (const closedNames') body
              pure $ KNClosure (KNLambdaF' args body') (map EVarLocal closedNames)
          -- Ignore the free vars, evaluate in-order, then weaken outer Sum
          _ -> Fix . weaken' <$> sequence (fmap snd adt)

