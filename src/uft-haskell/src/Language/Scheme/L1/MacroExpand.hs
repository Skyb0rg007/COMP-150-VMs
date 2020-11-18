{-# LANGUAGE TemplateHaskell #-}
module Language.Scheme.L1.MacroExpand
    ( 
    ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
-- import           Control.Monad.RWS.Strict
import           Data.Deriving
import           Data.Foldable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Scheme.L0.Ast     (L0)
import qualified Language.Scheme.L0.Ast     as L0
import           Language.Scheme.L1.Ast     (L1, Name)
import qualified Language.Scheme.L1.Ast     as L1
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Util

-- | Monad for macro expansion
type M a = StateT Int (ExceptT Text IO) a

-- | Generate a fresh name
gensym :: Name -> M Name
gensym (L0.Name base) = do
    n <- get
    put (n + 1)
    pure $ L1.N base (Just n)

data Denotation
    = GlobalVar Text
    | LocalVar Text Text
    | PatVar Text
    | Macro Text (Env -> OpenADT L0 -> M (OpenADT L1))

denotationName :: Denotation -> Text
denotationName (GlobalVar x) = x
denotationName (LocalVar x _) = x
denotationName (Macro x _) = x
denotationName (PatVar x) = x

type Env = HashMap Text Denotation

-- extendEnv :: [(Text, Denotation)] -> Env -> Env
-- extendEnv = HashMap.union . HashMap.fromList

-- applyEnv :: Env -> Text -> Denotation
-- applyEnv env name = fromMaybe (GlobalVar name) $ HashMap.lookup name env

-- mark :: Text -> Int -> Text
-- mark name mark = name <> "." <> tshow mark

-- unmark :: Env -> OpenADT L0 -> OpenADT L0
-- unmark env = cata alg where
    -- alg :: Sum L0 (OpenADT L0) -> OpenADT L0
    -- alg = \case
        -- L0.SymbolF s -> L0.Symbol . L0.Name $ denotationName (applyEnv env s)
        -- e -> Fix e


-- -- | Macro expansion environment
-- type Env = HashMap Name Denotation

-- -- | Types of identifiers
-- data Denotation
    -- = Special Text (OpenADT L0 -> M (OpenADT L1))
    -- -- ^ Predefined / primitive syntax
    -- | Macro [(OpenADT L0, OpenADT L0)] Env
    -- -- ^ User defined syntax
    -- | Identifier Name
    -- -- ^ Bound identifier
    -- | PatVar Name
    -- -- ^ Unbound identifier (used in lhs of clauses)

-- instance Eq Denotation where
    -- Special a _ == Special b _ = a == b
    -- Macro c e == Macro c' e' = c == c' && e == e'
    -- Identifier x == Identifier y = x == y
    -- -- PatVars are not equal
    -- _ == _ = False

-- -- | Lookup an identifier
-- macroLookup :: Env -> Name -> Denotation
-- macroLookup env name = fromMaybe (PatVar name) (HashMap.lookup name env)

-- -- | Bind an identifier
-- macroBind :: Env -> Name -> Denotation -> Env
-- macroBind env name d = HashMap.insert name d env

-- -- | Merge two environments
-- macroDivert :: Env -> Env -> Env
-- macroDivert env1 env2 = HashMap.union env2 env1

-- -- | Match an sexpression against a pattern
-- macroMatch
    -- :: OpenADT L0
    -- -> OpenADT L0
    -- -> Env
    -- -> Env
    -- -> Maybe (HashMap Name (OpenADT L0))
-- macroMatch l0 pat envUse envDef =
    -- case (l0, pat) of
      -- (_, L0.Symbol x')
        -- | PatVar v <- macroLookup envDef x' -> Just (HashMap.singleton v l0)
      -- (L0.Symbol x, L0.Symbol x')
        -- | macroLookup envDef x' == macroLookup envUse x -> Just HashMap.empty
      -- (L0.Pair e1 e2, L0.Pair π1 π2) ->
          -- case (macroMatch e1 π1 envUse envDef, macroMatch e2 π2 envUse envDef) of
            -- (Just m1, Just m2) -> Just (HashMap.union m1 m2)
            -- _ -> Nothing
      -- -- TODO: Literals
      -- _ -> Nothing

-- -- | Match a clause
-- macroTranscribe
    -- :: OpenADT L0
    -- -> [(OpenADT L0, OpenADT L0)]
    -- -> Env
    -- -> Env
    -- -> M (OpenADT L0, Env)
-- macroTranscribe l0 [] envUse envDef =
    -- throwError $ "Invalid syntax: " <> renderText (L0.prettyL0 l0)
-- macroTranscribe l0 ((π, ρ) : τ') envUse envDef =
    -- case macroMatch l0 π envUse envDef of
      -- Nothing -> macroTranscribe l0 τ' envUse envDef
      -- Just σ  -> do
          -- (l0', envNew) <- macroRewrite ρ σ envDef
          -- pure (l0', macroDivert envUse envNew)

-- -- | Rewrite the s-expression
-- macroRewrite
    -- :: OpenADT L0
    -- -> HashMap Name (OpenADT L0)
    -- -> Env
    -- -> M (OpenADT L0, Env)
-- macroRewrite ρ σ envDef = do
    -- let xs :: HashMap Name ()
        -- xs = HashSet.toMap (freeVars ρ)
    -- -- Mapping from varname to fresh varname
    -- xs' <- HashMap.traverseWithKey (\(L0.N n _) _ -> L0.Symbol <$> gensym n) xs
    -- let σ' = xs' <> σ
        -- envNew = 
    -- undefined

-- freeVars :: OpenADT L0 -> HashSet Name
-- freeVars = cata alg where
    -- alg = \case
        -- L0.SymbolF x -> HashSet.singleton x
        -- x            -> fold x

-- -- | Macro expansion
-- macroExpand
    -- :: Env
    -- -> OpenADT L0
    -- -> M (OpenADT L1)
-- macroExpand env l0 =
    -- case l0 of
      -- L0.Symbol s 
        -- | Identifier x <- macroLookup env s -> pure $ L1.Symbol x
      -- L0.Pair (L0.Symbol k) args 
        -- | Macro clauses env' <- macroLookup env k -> do
            -- (adt, env'') <- macroTranscribe l0 clauses env env'
            -- macroExpand env'' adt
        -- | Special _ f <- macroLookup env k -> f l0
      -- L0.List es -> L0.List <$> traverse (macroExpand env) es
      -- L0.Char c -> pure $ L1.Char c
      -- L0.String s -> pure $ L1.String s
      -- _ -> error "NYI"
    


