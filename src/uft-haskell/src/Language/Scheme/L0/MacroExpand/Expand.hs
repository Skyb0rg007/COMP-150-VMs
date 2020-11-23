
module Language.Scheme.L0.MacroExpand.Expand where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.HashMap.Strict                      (HashMap)
import qualified Data.HashMap.Strict                      as HashMap
import           Data.Text                                (Text)
import           Language.Scheme.L0.Ast
import qualified Language.Scheme.L0.MacroExpand.SyntaxEnv as S
import           Type.OpenADT
import           Data.Functor ((<&>))

type M a = StateT Int (ExceptT Text IO) a

exPat :: S.Pat
exPat = S.PatPair foo bar
    where
        foo = S.PatVar (S.PatternVar "foo" 0)
        bar = S.PatVar (S.PatternVar "bar" 0)

exSexp :: OpenADT L0
exSexp = List [Char 'a', Char 'b', Char 'c']

match
    :: S.Pat
    -> OpenADT L0
    -> S.Env
    -> S.Env
    -> Maybe (HashMap S.PatternVar (OpenADT L0))
match pat e envDef envUse = go pat e 0
    where
        go :: S.Pat
           -> OpenADT L0
           -> Int
           -> Maybe (HashMap S.PatternVar (OpenADT L0))
        go pat e rank =
            case (pat, e) of
              (S.PatEmpty, Empty) -> Just HashMap.empty
              (S.PatPair p1 p2, Pair e1 e2) -> do
                    m1 <- go p1 e1 rank
                    m2 <- go p2 e2 rank
                    Just (m1 <> m2)
              (S.PatSym x, Symbol x')
                | S.lookup x envDef == S.lookup x' envUse -> Just HashMap.empty
              (S.PatVar v, _) ->
                  Just $ HashMap.singleton v e
              (S.PatEllipsis p', _) -> go1 p' e (succ rank)
              (S.PatVector p, Vector v) -> go p (List v) rank
              (S.PatChar c, Char c') | c == c' -> Just HashMap.empty
              (S.PatBool b, Bool b') | b == b' -> Just HashMap.empty
              (S.PatNum n, Num n') | n == n' -> Just HashMap.empty
              _ -> Nothing
        go1 :: S.EllipsisPat
            -> OpenADT L0
            -> Int
            -> Maybe (HashMap S.PatternVar (OpenADT L0))
        go1 (S.EllipsisPat p vars) Empty rank =
            Just $ HashMap.fromList $ map (\v -> (v, Empty)) vars
        go1 (S.EllipsisPat p vars) (List es) rank = do
            answers <- traverse (\e -> go p e rank) es
            Just $ HashMap.fromList $ vars <&> \var ->
                (var, List $ map (HashMap.! var) answers)
        go1 _ _ _ = Nothing

