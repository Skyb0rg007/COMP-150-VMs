{-# LANGUAGE TemplateHaskell #-}
module Language.Scheme.L1.MacroExpand
    ( macroExpand
    , macroExpandAll
    , defaultEnv
    , Denotation (..)
    , M
    , runM
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logic
import           Control.Monad.Trans
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Bifunctor                   (first)
import           Data.Deriving
import           Data.Foldable
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.HashSet                     (HashSet)
import qualified Data.HashSet                     as HashSet
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Language.Scheme.L0.Ast
-- import qualified Language.Scheme.L0.Ast     as L0
-- import           Language.Scheme.L1.Ast     (L1, Name)
-- import qualified Language.Scheme.L1.Ast     as L1
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Debug.Trace
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Util

-- | Monad for macro expansion
newtype M a = M { unM :: StateT Int (ExceptT Text IO) a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

runM :: M a -> IO (Either Text a)
runM = runExceptT . flip evalStateT 0 . unM

testM :: M (OpenADT L0) -> IO ()
testM m = runM m >>= \case
    Left err -> putStrLn $ Text.unpack err
    Right x  -> putStrLn $ Text.unpack (renderText (prettyL0 x))

macroExpandAll
    :: [OpenADT L0]
    -> M [OpenADT L0]
macroExpandAll es =
    evalStateT (traverse (StateT . flip macroExpand) es) defaultEnv

expansionError :: Text -> M a
expansionError = M . lift . throwE

freshId :: M Int
freshId = M $ do
    n <- get
    put (n + 1)
    pure n

-- | Generate a fresh name using the given one as the base
gensym :: Name -> M Name
gensym (Name base) = do
    n <- freshId
    pure $ N base (Just n)

------------------------------------------------------------------------------

data Pat
    -- * Variables
    = PatIdentifier Name -- x
    | PatWild            -- _
    -- * Lists
    | PatList [Pat] -- (<pat>*)
    | PatDotList (NonEmpty Pat) Pat -- (<pat>+ . <pat>)
    | PatList' [Pat] PatRepeat [Pat] -- (<pat>* <pat> <ellipses> . <pat>)
    | PatDotList' [Pat] PatRepeat [Pat] Pat -- (<pat>* <pat> <ellipses> . <pat>)
    -- * Vectors
    | PatVector [Pat] -- #(<pat>*)
    | PatVector' [Pat] PatRepeat [Pat] -- #(<pat>* <pat> <ellipses> <pat>*)
    -- * Datum
    | PatSymbol Name -- x
    | PatString Text -- "str"
    | PatChar Char   -- 'c'
    | PatBool Bool   -- #t
    | PatNum Double  -- 3.14
    deriving Show

-- Matches 0 or more repetitions of pattern, binding result to the name
data PatRepeat = PatRepeat Name Pat -- <pat> <ellipses>
    deriving Show

matchPat
    :: Pat
    -> OpenADT L0
    -> Maybe (HashMap Name (OpenADT L0))
matchPat = go Just Nothing where
    -- Matches 0 or more 
    matchMany
        :: Pat
        -> [OpenADT L0]
        -> [OpenADT L0]
    matchMany pat es = error "Multiple matches NYI"

    go :: (HashMap Name (OpenADT L0) -> a) -- on-success
       -> a                                -- on-error
       -> Pat -> OpenADT L0                -- input pattern and template
       -> a
    go s f = curry $ \case
        (PatIdentifier x, e) -> s $ HashMap.singleton x e
        (PatWild, _)         -> s HashMap.empty
        (PatList [], List []) -> s HashMap.empty
        (PatList (p:ps), List (e:es)) ->
            let s' m = go (s . HashMap.union m) f (PatList ps) (List es)
             in go s' f p e
        (PatDotList p1 p2, Pair e1 e2) ->
            case NonEmpty.uncons p1 of
              (p, Nothing) ->
                  let s' m = go (s . HashMap.union m) f p2 e2
                   in go s' f p e1
              (p, Just ps) ->
                  let s' m = go (s . HashMap.union m) f (PatDotList ps p2) e2
                   in go s' f p e1
        (PatVector [], Vector []) -> s HashMap.empty
        (PatVector (p:ps), Vector (e:es)) ->
            let s' m = go (s . HashMap.union m) f (PatVector ps) (Vector es)
             in go s' f p e
        -- (PatVector' [] (PatRepeat n p) ps, Vector es)
        (PatSymbol x, Symbol x') | x == x' -> s HashMap.empty
        (PatString x, String x') | x == x' -> s HashMap.empty
        (PatChar c, Char c') | c == c' -> s HashMap.empty
        (PatNum n, Num n') | n == n' -> s HashMap.empty
        _ -> f
        -- (PatDotList (p1:|p1') p2, Pair e1 e2) ->
            -- let s' m = go (s . HashMap.union m) f (PatDotList p1' p2, e2)

data Denotation
    = GlobalVar                                                -- Global variable, not renamed
    | LocalVar Name                                            -- Local variable, renamed
    | PrimMacro Int (Env -> OpenADT L0 -> M (OpenADT L0, Env)) -- Primitive macro
    | UserMacro Int [(OpenADT L0, OpenADT L0)] Env             -- User-defined macro

instance Eq Denotation where
    (==) = curry $ \case
        (GlobalVar, GlobalVar) -> True
        (LocalVar a, LocalVar b) -> a == b
        (PrimMacro a _, PrimMacro b _) -> a == b
        (UserMacro a _ _, UserMacro b _ _) -> a == b
        _ -> False

instance Show Denotation where
    show GlobalVar          = "GlobalVar"
    show (LocalVar x)       = "(LocalVar " ++ show (unName x) ++ ")"
    show (PrimMacro id _)   = "(PrimMacro " ++ show id ++ ")"
    show (UserMacro id _ _) = "(UserMacro " ++ show id ++ ")"

type Env = HashMap Name Denotation

macroExpand'
    :: Traversable f
    => Env
    -> f (OpenADT L0)
    -> M (f (OpenADT L0), Env)
macroExpand' env es =
    runStateT (traverse (StateT . flip macroExpand) es) env

macroExpand
    :: Env
    -> OpenADT L0
    -> M (OpenADT L0, Env)
macroExpand env l0 = 
  -- trace ("expanding " ++ show (prettyL0 l0)) $
  case l0 of
    Symbol x ->
        -- trace ("looking up " ++ show (unName x) ++ " in " ++ show (map (first unName) $ HashMap.toList env)) $
        -- trace ("  -> " ++ show (HashMap.lookup x env)) $
        case HashMap.lookup x env of 
          Just (LocalVar v) -> pure (Symbol v, env)
          Just GlobalVar    -> pure (Symbol $ Name (_name_base x), env)
          Nothing           -> pure (Symbol x, env)
          _                 -> expansionError $ "Invalid syntax: " <> _name_base x
    e@(Pair (Symbol k) _)
      | Just (PrimMacro _ f) <- HashMap.lookup k env -> f env e
      | Just (UserMacro _ clauses env') <- HashMap.lookup k env -> do
          (x, env'') <- transcribe e clauses env env'
          macroExpand env'' x
    Pair a b -> do
        (a', env') <- macroExpand env a
        (b', env'') <- macroExpand env' b
        pure (Pair a' b', env'')
    Char c   -> pure (Char c, env)
    String s -> pure (String s, env)
    Bool b   -> pure (Bool b, env)
    Num n    -> pure (Num n, env)
    Empty    -> pure (Empty, env)
    _ -> expansionError "NYI"

transcribe
    :: OpenADT L0
    -> [(OpenADT L0, OpenADT L0)]
    -> Env
    -> Env
    -> M (OpenADT L0, Env)
transcribe sexp [] _ _ = expansionError $ "Invalid syntax: " <> renderText (prettyL0 sexp)
transcribe sexp ((π, ρ) : τ') eUse eDef =
    case match sexp π eUse eDef of
      Nothing -> transcribe sexp τ' eUse eDef
      Just σ  -> rewrite ρ σ eDef

match
    :: OpenADT L0
    -> OpenADT L0
    -> Env
    -> Env
    -> Maybe (HashMap Name (OpenADT L0))
match e (Symbol v) eUse eDef 
  | Nothing <- HashMap.lookup v eDef = Just (HashMap.singleton v e)
match (Symbol x) (Symbol x') eUse eDef 
  | HashMap.lookup x eUse == HashMap.lookup x' eDef = Just HashMap.empty
match (Pair e1 e2) (Pair p1 p2) eUse eDef
  | Just m1 <- match e1 p1 eUse eDef
  , Just m2 <- match e2 p2 eUse eDef = Just (m1 <> m2)
match _ _ _ _ = Nothing

rewrite
    :: OpenADT L0
    -> HashMap Name (OpenADT L0)
    -> Env
    -> M (OpenADT L0, Env)
rewrite ρ σ eDef = do
    let xs = freeVars ρ
    xsMapping <- HashMap.fromList . zip xs <$> traverse gensym xs
    let σ' = fmap Symbol xsMapping <> σ
    let eNew = flip HashMap.mapMaybeWithKey eDef $ \k v ->
            if k `elem` xs
               then Just (eDef HashMap.! (xsMapping HashMap.! k))
               else Nothing
    let rewrite' (Symbol x) σ
          | Just e <- HashMap.lookup x σ = pure e
          | otherwise = expansionError $ "Unmapped variable " <> tshow x
        rewrite' (Pair a b) σ = Pair <$> rewrite' a σ <*> rewrite' b σ
        rewrite' e _ = pure e
    (,eNew) <$> rewrite' ρ σ'

freeVars
    :: OpenADT L0
    -> [Name]
freeVars = HashSet.toList . cata alg where
    alg = \case
        SymbolF x -> HashSet.singleton x
        x         -> fold x

defaultEnv :: Env
defaultEnv = HashMap.fromList
    [ ("lambda", PrimMacro 0 lambdaMacro)
    , ("define", PrimMacro 1 defineMacro)
    ]
    where
        isSym :: OpenADT L0 -> Maybe Name
        isSym (Symbol x) = Just x
        isSym _          = Nothing
        lambdaMacro :: Env -> OpenADT L0 -> M (OpenADT L0, Env)
        lambdaMacro env (List (Symbol "lambda" : List args : body))
          | Just args' <- traverse isSym args = do
              freshArgs <- traverse gensym args'
              let env' = HashMap.fromList (zip args' (LocalVar <$> freshArgs)) <> env
              (body', _) <- macroExpand' env' body
              pure (List (Symbol "lambda" : List (map Symbol freshArgs) : body'), env)
        lambdaMacro _ _ = expansionError "Invalid lambda syntax"
        defineMacro :: Env -> OpenADT L0 -> M (OpenADT L0, Env)
        defineMacro env (List [Symbol "define", Symbol name, e]) = do
            name' <- gensym name
            let env' = HashMap.insert name (LocalVar name') env
            (e', _) <- macroExpand env' e
            pure (List [Symbol "define", Symbol name', e], env')
        defineMacro env (List [Symbol "define", List (Symbol name : args), e])
          | Just args' <- traverse isSym args = do
              name' <- gensym name
              freshArgs <- traverse gensym args'
              let env' = HashMap.fromList ((name, LocalVar name') : zip args' (map LocalVar freshArgs)) <> env
              (e', _) <- macroExpand env' e
              pure (List [Symbol "define", List $ Symbol name' : map Symbol freshArgs, e'], env')
        defineMacro _ _ = expansionError "Invalid define syntax"

