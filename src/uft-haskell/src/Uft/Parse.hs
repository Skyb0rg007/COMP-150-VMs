{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Uft.Parse
    ( ParseF (parseF')
    , parseF
    , parseFTest
    , parseFEither
    , ParseContext (PCTop, PCExp, PCQuasi, PCQuote, PCQ, PCNotQ)
    , module Uft.SExpr.Types
    , module Control.Applicative
    , module Control.Monad
    ) where

import           Control.Applicative (Alternative (empty, (<|>)))
import           Control.Monad       (MonadPlus (mplus, mzero))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Type.OpenADT
import           Uft.Pretty
import           Uft.SExpr.Parse     (parseSExpr)
import           Uft.SExpr.Types     (SAtom (..), SExpr (..))

data ParseContext
    = PCTop              -- ^ Toplevel definition
    | PCExp              -- ^ Expression
    | PCQuasi !Int !Bool -- ^ Quasiquote level, allow unquote-splicing
    | PCQuote            -- ^ Quoted
    deriving (Show, Eq, Ord, Read)

pattern PCNotQ :: ParseContext
pattern PCNotQ <- (\case { PCTop -> True; PCExp -> True; _ -> False } -> True)
{-# COMPLETE PCNotQ, PCQ #-}

pattern PCQ :: ParseContext
pattern PCQ <- (\case { PCTop -> False; PCExp -> False; _ -> True } -> True)

data ParseResult a
    = PROk a
    | PRNone
    | PRError String
    deriving (Show, Eq, Ord, Read, Functor)

instance Applicative ParseResult where
    pure = PROk
    PROk f <*> PROk x = PROk (f x)
    PRError e <*> _ = PRError e
    _ <*> PRError e = PRError e
    PRNone <*> _ = PRNone
    _ <*> PRNone = PRNone

instance Alternative ParseResult where
    empty = PRNone
    PRError e <|> _ = PRError e
    _ <|> PRError e = PRError e
    PRNone <|> x = x
    x <|> _ = x

instance Monad ParseResult where
    PROk x    >>= f = f x
    PRError e >>= _ = PRError e
    PRNone    >>= _ = PRNone

instance MonadFail ParseResult where
    fail = PRError

instance MonadPlus ParseResult where
    mzero = empty
    mplus = (<|>)

class ParseF f where
    parseF' :: (MonadPlus m, MonadFail m)
            => ParseContext
            -> SExpr
            -> m (f (ParseContext, SExpr))

instance Apply ParseF r => ParseF (Sum r) where
    parseF' pc datum = coapply @ParseF (parseF' pc datum)

parseF :: forall r. Applies '[Functor, Foldable, Traversable, ParseF] r
       => ParseContext
       -> SExpr
       -> ParseResult (OpenADT r)
parseF = curry (anaM (uncurry parseF'))

parseFEither :: forall r. Applies '[Functor, Foldable, Traversable, ParseF] r
             => ParseContext
             -> SExpr
             -> Either String (OpenADT r)
parseFEither pc e =
    case parseF pc e of
      PROk x -> Right x
      PRNone -> Left "No parse"
      PRError e -> Left e

parseFTest :: forall r. Applies '[Functor, Foldable, Traversable, ParseF, PrettyF] r
           => Text
           -> IO ()
parseFTest input =
    case parseSExpr "" input of
      Left err -> putStrLn (Text.unpack err)
      Right sexp ->
          case parseF @r PCTop sexp of
            PROk datum -> print $ prettyF datum
            PRNone -> putStrLn "No parse"
            PRError e -> putStrLn $ "Parsing error: " ++ e

