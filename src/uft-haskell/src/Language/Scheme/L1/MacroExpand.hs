{-# LANGUAGE TemplateHaskell #-}
module Language.Scheme.L1.MacroExpand
    ( macroExpand
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

testEnv :: Env
testEnv = HashMap.fromList
    [ ("x", UserMacro 0 [(Pair (Symbol "x") (Symbol "y"), List [Symbol "y", Symbol "y"])] mempty)
    , ("y", GlobalVar)
    ]

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
matchPat = curry (go Just Nothing) where
    -- Matches 0 or more 
    matchMany
        :: Pat
        -> [OpenADT L0]
        -> [OpenADT L0]
    matchMany pat es = []
    go :: (HashMap Name (OpenADT L0) -> a)
       -> a
       -> (Pat, OpenADT L0)
       -> a
    go s f = \case
        (PatIdentifier x, e) -> s $ HashMap.singleton x e
        (PatWild, _)         -> s HashMap.empty
        (PatList [], List []) -> s HashMap.empty
        (PatList (p:ps), List (e:es)) ->
            let s' m = go (s . HashMap.union m) f (PatList ps, List es)
             in go s' f (p, e)
        (PatDotList p1 p2, Pair e1 e2) ->
            case NonEmpty.uncons p1 of
              (p, Nothing) ->
                  let s' m = go (s . HashMap.union m) f (p2, e2)
                   in go s' f (p, e1)
              (p, Just ps) ->
                  let s' m = go (s . HashMap.union m) f (PatDotList ps p2, e2)
                   in go s' f (p, e1)
        (PatVector [], Vector []) -> s HashMap.empty
        (PatVector (p:ps), Vector (e:es)) ->
            let s' m = go (s . HashMap.union m) f (PatVector ps, Vector es)
             in go s' f (p, e)
        -- (PatVector' [] (PatRepeat n p) ps, Vector es)
        (PatSymbol x, Symbol x') | x == x' -> s HashMap.empty
        (PatString x, String x') | x == x' -> s HashMap.empty
        (PatChar c, Char c') | c == c' -> s HashMap.empty
        (PatNum n, Num n') | n == n' -> s HashMap.empty
        _ -> f
        -- (PatDotList (p1:|p1') p2, Pair e1 e2) ->
            -- let s' m = go (s . HashMap.union m) f (PatDotList p1' p2, e2)

exPat :: Pat
exPat = PatList [PatDotList (PatIdentifier "x":|[]) (PatIdentifier "y"), PatString "foo"]

exInput :: OpenADT L0
exInput = List [List [Char 'c', Char 'd', Char 'e'], String "foo"]

-- matchPat
    -- :: Pat
    -- -> OpenADT L0
    -- -> (HashMap Name (OpenADT L0) -> a) -- Success
    -- -> a -- Failure
    -- -> a
-- matchPat PatWild _ success _ = success HashMap.empty
-- matchPat (PatSym x) e success failure =
    -- case e of
      -- Symbol x'
        -- | x == x' -> success HashMap.empty
      -- _           -> failure
-- matchPat (PatVar x) e success _ = success (HashMap.singleton x e)
-- matchPat PatEmpty Empty success _ = success HashMap.empty
-- matchPat (PatPair p1 p2) e success failure =
    -- case e of
      -- Pair e1 e2 ->
          -- let success' m = matchPat p2 e2 (success . HashMap.union m) failure
           -- in matchPat p1 e1 success' failure
      -- _ -> failure
-- matchPat PatEmpty e success failure =
    -- case e of
      -- Empty -> success HashMap.empty
      -- _     -> failure
-- matchPat PatRepeat{} _ _ failure = failure

-- matchPat :: Pat -> OpenADT L0 -> Maybe (HashMap Name (OpenADT L0))
-- matchPat PatWild _ = pure HashMap.empty
-- matchPat (PatSym x) (Symbol x')
  -- | x == x' = pure HashMap.empty
-- matchPat PatSym{} _ = mzero
-- matchPat (PatVar x) e = pure $ HashMap.singleton x e
-- matchPat (PatList ps) (List es)
  -- | length ps == length es = fmap mconcat $ traverse (uncurry matchPat) (zip ps es)
  -- | otherwise = mzero
-- matchPat (PatRepeat p) e = mzero

data Denotation
    = GlobalVar                                       -- Global variable, not renamed
    | LocalVar Name                                   -- Local variable, renamed
    | PrimMacro Int (Env -> OpenADT L0 -> M (OpenADT L0)) -- Primitive macro
    | UserMacro Int [(OpenADT L0, OpenADT L0)] Env        -- User-defined macro

instance Eq Denotation where
    GlobalVar == GlobalVar = True
    LocalVar a == LocalVar b = a == b
    PrimMacro a _ == PrimMacro b _ = a == b
    UserMacro a _ _ == UserMacro b _ _ = a == b
    _ == _ = False

instance Show Denotation where
    show GlobalVar = "GlobalVar"
    show (LocalVar x) = "(LocalVar " ++ show (unName x) ++ ")"
    show (PrimMacro id _) = "(PrimMacro " ++ show id ++ ")"
    show (UserMacro id _ _) = "(UserMacro " ++ show id ++ ")"

type Env = HashMap Name Denotation

macroExpand
    :: Env
    -> OpenADT L0
    -> M (OpenADT L0)
macroExpand env l0 = 
  -- trace ("expanding " ++ show (prettyL0 l0)) $
  case l0 of
    Symbol x ->
        -- trace ("looking up " ++ show (unName x) ++ " in " ++ show (map (first unName) $ HashMap.toList env)) $
        -- trace ("  -> " ++ show (HashMap.lookup x env)) $
        case HashMap.lookup x env of 
          Just (LocalVar v) -> pure $ Symbol v
          Just GlobalVar    -> pure $ Symbol $ Name (_name_base x)
          Nothing           -> pure $ Symbol x
          _                 -> expansionError $ "Invalid syntax: " <> _name_base x
    e@(Pair (Symbol k) _)
      | Just (PrimMacro _ f) <- HashMap.lookup k env -> f env e
      | Just (UserMacro _ clauses env') <- HashMap.lookup k env -> do
          (x, env'') <- transcribe e clauses env env'
          macroExpand env'' x
    Pair a b -> Pair <$> macroExpand env a <*> macroExpand env b
    Char c   -> pure $ Char c
    String s -> pure $ String s
    Bool b   -> pure $ Bool b
    Num n    -> pure $ Num n
    Empty    -> pure Empty
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
    ]
    where
        isSym :: OpenADT L0 -> Maybe Name
        isSym (Symbol x) = Just x
        isSym _          = Nothing
        lambdaMacro :: Env -> OpenADT L0 -> M (OpenADT L0)
        lambdaMacro env (List (Symbol "lambda" : List args : body))
          | Just args' <- traverse isSym args = do
              freshArgs <- traverse gensym args'
              let env' = HashMap.fromList (zip args' (LocalVar <$> freshArgs)) <> env
              body' <- traverse (macroExpand env') body
              -- traceM $ "body = " ++ show body
              -- traceM $ "body' = " ++ show body'
              pure $ List (Symbol "lambda" : List (map Symbol freshArgs) : body')
        lambdaMacro _ _ = expansionError "Invalid lambda syntax"

