{-# OPTIONS_GHC -Wall #-}
-- TODO: quasiquote
module Language.Scheme.L0
    ( L0 (..)
    ) where

import           Control.Lens               (preview)
import           Control.Monad              (replicateM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Bifunctor             (first)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import           Data.Unique
import           Language.Scheme.Primitives
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Class
import           Language.Scheme.Util
import           System.IO.Unsafe           (unsafePerformIO)

data L0
    = EDatum SExp
    | ELet [(Unique, L0)] L0
    | ELetRec [(Unique, L0)] L0
    | ELambda [Unique] L0
    | EBegin [L0]
    | ELocalSet Unique L0
    | EGlobalSet Text L0
    | EIf L0 L0 L0
    | EWhile L0 L0
    | ELocalVar Unique
    | EGlobalVar Text
    | EApply L0 [L0]
    | EPrimApply Prim [L0]

instance Embed L0 where
    embed = \case
        EDatum sexp       -> SList ["quote", sexp]
        ELet bs e         -> SList ["let", SList (embedBinds bs), embed e]
        ELetRec bs e      -> SList ["letrec", SList (embedBinds bs), embed e]
        ELambda args e    -> SList ["lambda", SList (map unique args), embed e]
        EBegin es         -> SList ("begin" : map embed es)
        ELocalSet x e     -> SList ["set!", unique x, embed e]
        EGlobalSet x e    -> SList ["set-global!", SSymbol x, embed e]
        EIf e1 e2 e3      -> SList ["if", embed e1, embed e2, embed e3]
        EWhile e1 e2      -> SList ["while", embed e1, embed e2]
        ELocalVar x       -> unique x
        EGlobalVar x      -> SList ["global", SSymbol x]
        EApply f args     -> SList (embed f : map embed args)
        EPrimApply p args -> SList ("prim" : SSymbol (_prim_name p) : map embed args)
        where
            unique u = SSymbol $ "x" <> tshow (hashUnique u)
            embedBinds = map $ \(x, e) -> SList [unique x, embed e]

-- * Projection

data Denotation
    = DLocal Unique
    | DGlobal
    | DSpecial (SExp -> M L0)

instance Show Denotation where
    showsPrec d = \case
        DLocal u   -> showParen (d > 10) $ showString "DLocal " . shows (hashUnique u)
        DGlobal    -> showString "DGlobal"
        DSpecial{} -> showParen (d > 10) $ showString "DSpecial <procedure>"

type Env = HashMap Text Denotation

newtype M a = M (StateT Env (ExceptT Text IO) a)
    deriving newtype (Functor, Applicative, Monad)

runM :: M a -> Env -> Either Text a
runM (M m) env = unsafePerformIO $ runExceptT $ m `evalStateT` env

getEnv :: M Env
getEnv = M get

addGlobal :: Text -> M ()
addGlobal name = M $ modify' (HashMap.insert name DGlobal)

withLocal :: Text -> (Unique -> M a) -> M a
withLocal x k = withLocals [x] (k . head)

withLocals :: [Text] -> ([Unique] -> M a) -> M a
withLocals names k = M $ do
    s <- get
    us <- replicateM (length names) (liftIO newUnique)
    put (HashMap.fromList (zip names (map DLocal us)) <> s)
    x <- case k us of M m -> m
    put s
    pure x

prjError :: Text -> M a
prjError = M . lift . throwE

--

instance Project L0 where
    project = flip runM defaultEnv . traverse go where
        begin :: [L0] -> L0
        begin [x] = x
        begin es  = EBegin es

        letrec :: [(Unique, L0)] -> L0 -> L0
        letrec [] = id
        letrec bs = ELetRec bs

        etaExpand :: Prim -> M L0
        etaExpand p =
            let arity = _prim_inputArity p
                args = take arity ((\n -> "x" <> tshow @Int n) <$> [0 ..])
             in withLocals args $ \args' ->
                 pure $ ELambda args' (EPrimApply p (map ELocalVar args'))

        -- TODO: Handle local defines
        projectLambda :: SExp -> M L0
        projectLambda = \case
            SList (_ : SList (traverse (preview _SSymbol) -> Just args) : body) ->
                withLocals args $ \args' -> do
                    body' <- projectBody body
                    pure $ ELambda args' body'
            _ -> prjError "Invalid lambda syntax"

        projectBody :: [SExp] -> M L0
        projectBody [] = prjError "Empty bodies not allowed"
        projectBody sexps = do
            env <- getEnv
            let isDefine :: SExp -> M (Maybe (Text, M L0))
                isDefine = 
                    case HashMap.lookup "define" env of
                      Just DSpecial{} -> \case
                        SList ("define" : SList (traverse (preview _SSymbol) -> Just (name : args)) : body) -> do
                            pure $ Just $ (name,) $
                                withLocals args $ \args' -> do
                                    body' <- projectBody body
                                    pure $ ELambda args' body'
                        SList ["define", SSymbol x, e] ->
                            pure $ Just (x, go e)
                        SPair "define" _ -> prjError "Invalid local define syntax"
                        _ -> pure Nothing
                      _ -> const $ pure Nothing
            let split :: [SExp] -> M ([(Text, M L0)], [SExp])
                split [] = pure ([], [])
                split (e:es) = isDefine e >>= \case
                    Nothing -> pure ([], e:es)
                    Just d  -> first (d:) <$> split es
            (binds, body) <- split sexps
            -- withExtendEnv (HashMap.fromList (map (,DLocal) (map fst binds))) $
            withLocals (map fst binds) $ \names ->
                letrec <$> (zip names <$> sequence (map snd binds)) <*> fmap begin (traverse go body)

        projectBegin :: SExp -> M L0
        projectBegin (SList (_ : es)) = EBegin <$> traverse go es
        projectBegin _ = prjError "Invalid begin syntax"

        projectQuote :: SExp -> M L0
        projectQuote (SList [_, x]) = pure $ EDatum x
        projectQuote _ = prjError "Invalid quote syntax"

        projectDefine :: SExp -> M L0
        projectDefine = \case
            SList (_ : SList (traverse (preview _SSymbol) -> Just (name : args)) : body) -> do
                addGlobal name
                withLocals args $ \args' -> do
                    body' <- projectBody body
                    pure $ EGlobalSet name $ ELambda args' body'
            SList [_, SSymbol name, e] -> do
                addGlobal name
                e' <- go e
                pure $ EGlobalSet name e'
            _ -> prjError "Invalid define syntax"

        projectSet :: SExp -> M L0
        projectSet sexp = do
            env <- getEnv
            case sexp of
              SList [_, SSymbol x, e]
                | Just (DLocal u) <- HashMap.lookup x env -> ELocalSet u <$> go e
                | Just DGlobal <- HashMap.lookup x env -> EGlobalSet x <$> go e
                | Nothing <- HashMap.lookup x env -> EGlobalSet x <$> go e
              _ -> prjError "Invalid set! syntax"

        projectIf :: SExp -> M L0
        projectIf = \case
            SList [_, e1, e2, e3] -> EIf <$> go e1 <*> go e2 <*> go e3
            SList [_, e1, e2] -> EIf <$> go e1 <*> go e2 <*> pure (EPrimApply (prim "void") [])
            _ -> prjError "Invalid if syntax"

        projectWhile :: SExp -> M L0
        projectWhile = \case
            SList (_ : e : es) -> EWhile <$> go e <*> fmap begin (traverse go es)
            _ -> prjError "Invalid while syntax"

        letBind :: SExp -> Maybe (Text, SExp)
        letBind (SList [SSymbol x, e]) = Just (x, e)
        letBind _ = Nothing

        projectLet :: SExp -> M L0
        projectLet = \case
            SList (_ : SList binds : body)
              | Just bs <- traverse letBind binds -> do
                  let (names, es) = unzip bs
                  es' <- traverse go es
                  withLocals names $ \names' -> do
                      body' <- projectBody body
                      pure $ ELet (zip names' es') body'
            _ -> prjError "Invalid let syntax"

        projectLetStar :: SExp -> M L0
        projectLetStar = \case
            SList (_ : SList binds : body)
              | Just bs <- traverse letBind binds -> do
                  let f :: M L0 -> (Text, SExp) -> M L0
                      f acc (x, e) =
                          withLocal x $ \x' -> do
                              e' <- go e
                              ELet [(x', e')] <$> acc
                   in foldl f (projectBody body) bs
            _ -> prjError "Invalid let* syntax"

        projectLetRec :: SExp -> M L0
        projectLetRec = \case
            SList (_ : SList binds : body)
              | Just bs <- traverse letBind binds -> do
                  let (names, es) = unzip bs
                  withLocals names $ \names' -> do
                      es' <- traverse go es
                      body' <- projectBody body
                      pure $ ELetRec (zip names' es') body'
            _ -> prjError "Invalid letrec syntax"

        -- Nmal letrec is expanded the same way as Scheme's letrec*
        projectLetRecStar :: SExp -> M L0
        projectLetRecStar = \case
            SList (_ : SList binds : body)
              | Just bs <- traverse letBind binds -> do
                  let (names, es) = unzip bs
                  withLocals names $ \names' -> do
                      es' <- traverse go es
                      body' <- projectBody body
                      pure $ ELetRec (zip names' es') body'
            _ -> prjError "Invalid letrec* syntax"

        projectWhen :: SExp -> M L0
        projectWhen = \case
            SList (_ : cond : rest) | not (null rest) ->
                EIf <$> go cond <*> fmap begin (traverse go rest) <*> pure (EPrimApply (prim "void") [])
            _ -> prjError "Invalid when syntax"

        projectUnless :: SExp -> M L0
        projectUnless = \case
            SList (_ : cond : rest) | not (null rest) ->
                EIf <$> go cond <*> pure (EPrimApply (prim "void") []) <*> fmap begin (traverse go rest)
            _ -> prjError "Invalid unless syntax"

        projectError :: Text -> SExp -> M L0
        projectError msg _ = prjError $ "Invalid " <> msg <> " syntax"

        -- projectQuasi :: SExp -> M L0
        -- projectQuasi sexp = do
            -- env <- getEnv
            -- let isSpecial x = HashMap.member x env
            -- let expand = \case
                    -- SList [SSymbol "unquote", x]
                      -- | isSpecial "unquote" -> pure $ EDatum x
                    -- SList [SSymbol "unquote-splicing", x]
                      -- | isSpecial "unquote-splicing" -> prjError "Invalid unquote-splicing syntax"
                    -- SList [SSymbol "quasiquote", x]
                      -- | isSpecial "quasiquote" -> expand 
                    -- _ -> prjError "Invalid quasiquote syntax"
            -- expand sexp

        defaultEnv :: Env
        defaultEnv = HashMap.fromList
            [ ("lambda",           DSpecial projectLambda)
            , ("begin",            DSpecial projectBegin)
            , ("quote",            DSpecial projectQuote)
            , ("define",           DSpecial projectDefine)
            , ("set!",             DSpecial projectSet)
            , ("if",               DSpecial projectIf)
            , ("while",            DSpecial projectWhile)
            , ("let",              DSpecial projectLet)
            , ("let*",             DSpecial projectLetStar)
            , ("letrec",           DSpecial projectLetRec)
            , ("letrec*",          DSpecial projectLetRecStar)
            , ("when",             DSpecial projectWhen)
            , ("unless",           DSpecial projectUnless)
            -- TODO
            , ("unquote",          DSpecial (projectError "unquote"))
            , ("unquote-splicing", DSpecial (projectError "unquote-splicing"))
            , ("quasiquote",       DSpecial (projectError "quasiquote"))
            ]

        go :: SExp -> M L0
        go sexp = do
            env <- getEnv
            case sexp of
              SSymbol x
                | Just DSpecial{} <- HashMap.lookup x env -> prjError $ "Invalid " <> x <> " synax"
                | Just (DLocal u) <- HashMap.lookup x env -> pure $ ELocalVar u
                | Nothing <- HashMap.lookup x env, Just p <- parsePrim x -> etaExpand p
                | otherwise -> pure $ EGlobalVar x
              SList (SSymbol x : args)
                | Just (DSpecial f) <- HashMap.lookup x env -> f sexp
                | Nothing <- HashMap.lookup x env, Just p <- parsePrim x ->
                    EPrimApply p <$> traverse go args
              SList (f:args) -> EApply <$> go f <*> traverse go args
              SChar{}       -> pure (EDatum sexp)
              SString{}     -> pure (EDatum sexp)
              SBool{}       -> pure (EDatum sexp)
              SByteVector{} -> pure (EDatum sexp)
              SNum{}        -> pure (EDatum sexp)
              SVector{}     -> prjError "Vector in non-quoted position"
              SEmpty        -> prjError "Empty-list in non-quoted position"
              SPair{}       -> prjError "Invalid improper list"


