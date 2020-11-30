
module Language.Scheme.L0.Ast
    ( module Language.Scheme.L0.Ast
    ) where

import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Except
import           Control.Lens               (preview)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Text                  (Text)
import           Language.Scheme.SExp.Ast
import           Data.Bifunctor (first)
import           Language.Scheme.SExp.Class
import           Uft.Primitives

data L0
    = EDatum SExp
    | EDefine Text L0
    | ELet [(Text, L0)] L0
    | ELetRec [(Text, L0)] L0
    | ELetRecStar [(Text, L0)] L0
    | ELambda [Text] L0
    | EBegin [L0]
    | ELocalSet Text L0
    | EGlobalSet Text L0
    | EIf L0 L0 L0
    | EWhile L0 L0
    | ELocalVar Text
    | EGlobalVar Text
    | EApply L0 [L0]
    | EPrimApply Prim [L0]

instance Embed L0 where
    embed = \case
        EDatum sexp       -> SList ["quote", sexp]
        EDefine x e       -> SList ["define", SSymbol x, embed e]
        ELet bs e         -> SList ["let", SList (embedBinds bs), embed e]
        ELetRec bs e      -> SList ["letrec", SList (embedBinds bs), embed e]
        ELetRecStar bs e  -> SList ["letrec*", SList (embedBinds bs), embed e]
        ELambda args e    -> SList ["lambda", SList (map SSymbol args), embed e]
        EBegin es         -> SList ("begin" : map embed es)
        ELocalSet x e     -> SList ["set!", SSymbol x, embed e]
        EGlobalSet x e    -> SList ["set-global!", SSymbol x, embed e]
        EIf e1 e2 e3      -> SList ["if", embed e1, embed e2, embed e3]
        EWhile e1 e2      -> SList ["while", embed e1, embed e2]
        ELocalVar x       -> SSymbol x
        EGlobalVar x      -> SList ["global", SSymbol x]
        EApply f args     -> SList (embed f : map embed args)
        EPrimApply p args -> SList ("prim" : SSymbol (_prim_name p) : map embed args)
        where
            embedBinds = map $ \(x, e) -> SList [SSymbol x, embed e]

-- * Projection

data Denotation
    = DLocal
    | DGlobal
    | DSpecial Text

type Env = HashMap Text Denotation

newtype M a = M (StateT Env (Except Text) a)
    deriving newtype (Functor, Applicative, Monad)

runM :: M a -> Env -> Either Text a
runM (M m) env = runExcept $ m `evalStateT` env

getEnv :: M Env
getEnv = M get

extendEnv :: Env -> M ()
extendEnv env = M $ modify' (env <>)

withExtendEnv :: Env -> M a -> M a
withExtendEnv env (M m) = M $ do
    s <- get
    put env
    x <- m
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
        letrecstar :: [(Text, L0)] -> L0 -> L0
        letrecstar [] = id
        letrecstar binds = ELetRecStar binds

        -- TODO: Handle local (define)
        projectLambda :: SExp -> M L0
        projectLambda x = do
            env <- getEnv
            case x of
              SList (_ : SList (traverse (preview _SSymbol) -> Just args) : body) ->
                  withExtendEnv (HashMap.fromList (map (,DLocal) args)) $ do
                      body' <- traverse go body
                      pure $ ELambda args $ begin body'
              _ -> prjError "Invalid lambda syntax"

        projectBegin :: SExp -> M L0
        projectBegin (SList (_ : es)) = EBegin <$> traverse go es
        projectBegin _ = prjError "Invalid begin syntax"

        projectQuote :: SExp -> M L0
        projectQuote (SList [_, x]) = pure $ EDatum x
        projectQuote _ = prjError "Invalid quote syntax"

        projectDefine :: SExp -> M L0
        projectDefine x = do
            env <- getEnv
            case x of
              SList (_ : SList (traverse (preview _SSymbol) -> Just (name : args)) : body) -> do
                  extendEnv $ HashMap.fromList (map (,DGlobal) args)
                  body' <- traverse go body
                  pure $ EDefine name $ ELambda args (begin body')
              SList [_, SSymbol name, e] -> do
                  extendEnv $ HashMap.singleton name DGlobal
                  e' <- go e
                  pure $ EDefine name e'
              _ -> prjError "Invalid define syntax"

        projectSet :: SExp -> M L0
        projectSet x = do
            env <- getEnv
            case x of
              SList [_, SSymbol x, e]
                | Just DLocal <- HashMap.lookup x env -> ELocalSet x <$> go e
                | Just DGlobal <- HashMap.lookup x env -> EGlobalSet x <$> go e
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
        projectLet sexp = do
            env <- getEnv
            case sexp of
              SList (_ : SList binds : body)
                | Just bs <- traverse letBind binds -> do
                    let env' = map ((,DLocal) . fst) bs
                    bs' <- traverse (traverse go) bs
                    body' <- withExtendEnv (HashMap.fromList env') $ traverse go body
                    pure $ ELet bs' (begin body')

        projectLetstar :: SExp -> M L0
        projectLetstar sexp = do
            env <- getEnv
            case sexp of
              SList (_ : SList binds : body)
                | Just bs <- traverse letBind binds -> do
                    let f :: M L0 -> (Text, SExp) -> M L0
                        f acc (x, e) =
                            withExtendEnv (HashMap.singleton x DLocal) $ do
                                e' <- go e
                                ELet [(x, e')] <$> acc
                     in foldl f (begin <$> traverse go body) bs

        projectLetrec :: SExp -> M L0
        projectLetrec sexp = do
            env <- getEnv
            case sexp of
              SList (_ : SList binds : body)
                | Just bs <- traverse letBind binds -> do
                    let env' = HashMap.fromList $ map ((,DLocal) . fst) bs
                    bs' <- withExtendEnv env' $ traverse (traverse go) bs
                    body' <- withExtendEnv env' $ traverse go body
                    pure $ ELetRec bs' (begin body')

        projectLetrecstar :: SExp -> M L0
        projectLetrecstar sexp = do
            env <- getEnv
            case sexp of
              SList (_ : SList binds : body)
                | Just bs <- traverse letBind binds -> do
                    let env' = HashMap.fromList $ map ((,DLocal) . fst) bs
                    bs' <- withExtendEnv env' $ traverse (traverse go) bs
                    body' <- withExtendEnv env' $ traverse go body
                    pure $ ELetRecStar bs' (begin body')

        defaultEnv :: Env
        defaultEnv = HashMap.fromList $ map (\x -> (x, DSpecial x)) $
            [ "lambda"
            , "begin"
            , "quote"
            , "define"
            , "set!"
            , "if"
            , "while"
            , "let"
            , "let*"
            , "letrec"
            , "letrec*"
            ]

        go :: SExp -> M L0
        go sexp = do
            env <- getEnv
            case sexp of
              SSymbol x
                | Just DGlobal <- HashMap.lookup x env -> pure $ EGlobalVar x
                | Just DSpecial{} <- HashMap.lookup x env -> prjError $ "Invalid " <> x <> " synax"
                | otherwise -> pure $ ELocalVar x
              SList (SSymbol x : _)
                | Just (DSpecial n) <- HashMap.lookup x env ->
                    case n of
                      "begin"   -> projectBegin      sexp
                      "lambda"  -> projectLambda     sexp
                      "quote"   -> projectQuote      sexp
                      "define"  -> projectDefine     sexp
                      "set!"    -> projectSet        sexp
                      "if"      -> projectIf         sexp
                      "while"   -> projectWhile      sexp
                      "let"     -> projectLet        sexp
                      "let*"    -> projectLetstar    sexp
                      "letrec"  -> projectLetrec     sexp
                      "letrec*" -> projectLetrecstar sexp
                      _ -> error $ "Invalid special denotation: " ++ show n
              SList (f:args) -> EApply <$> go f <*> traverse go args
              SChar{}       -> pure $ EDatum sexp
              SString{}     -> pure $ EDatum sexp
              SBool{}       -> pure $ EDatum sexp
              SByteVector{} -> pure $ EDatum sexp
              SNum{}        -> pure $ EDatum sexp
              SVector{}     -> prjError $ "Vector in non-quoted position"
              SEmpty        -> prjError $ "Empty-list in non-quoted position"
              SPair{}       -> prjError $ "Invalid improper list"


