
module Main
    ( main
    ) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.HashMap.Strict                   (HashMap)
import qualified Data.HashMap.Strict                   as HashMap
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as Text.IO
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Language.Scheme.L0                    (L0)
import           Language.Scheme.L1                    (L1)
import           Language.Scheme.L2                    (L2)
import           Language.Scheme.L3                    (L3)
import           Language.Scheme.L4                    (L4)
import           Language.Scheme.L5                    (L5)
import           Language.Scheme.L6                    (L6, compileObjProg)
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Class
import           Language.Scheme.SExp.Parse
-- import           Language.Scheme.L1.MacroExpand
import           System.Environment
import           System.Exit

putDocLn :: Doc ann -> IO ()
putDocLn x = putDoc x >> putStrLn ""

usage :: IO a
usage = do
    putStrLn "Usage: uft <lang> <file>"
    exitFailure

main :: IO ()
main = do
    args :: [String] <- getArgs
    (pipelineName :: Text, fileName :: FilePath) <-
        case args of
          [a, b] -> pure (Text.pack a, b)
          _ -> usage
    pipeline <- case HashMap.lookup pipelineName pipelines of
                  Nothing -> usage
                  Just p -> pure p
    fileContent <- Text.IO.readFile fileName
    case parseSExp fileName fileContent of
      Left err  -> putStrLn err
      Right sexps -> pipeline sexps

pipelines :: HashMap Text ([SExp] -> IO ())
pipelines = HashMap.fromList
    [ (,) "l0" $ \sexps -> do
        case project sexps :: Either Text [L0] of
          Left err -> Text.IO.putStrLn err
          Right res -> forM_ res (putDocLn . pretty . embed)
    , (,) "l1" $ \sexps -> do
        case project sexps :: Either Text [L1] of
          Left err -> Text.IO.putStrLn err
          Right res -> forM_ res (putDocLn . pretty . embed)
    , (,) "l2" $ \sexps -> do
        case project sexps :: Either Text [L2] of
          Left err -> Text.IO.putStrLn err
          Right res -> forM_ res (putDocLn . pretty . embed)
    , (,) "l3" $ \sexps -> do
        case project sexps :: Either Text [L3] of
          Left err -> Text.IO.putStrLn err
          Right res -> forM_ res (putDocLn . pretty . embed)
    , (,) "l4" $ \sexps -> do
        case project sexps :: Either Text [L4] of
          Left err -> Text.IO.putStrLn err
          Right res -> forM_ res (putDocLn . pretty . embed)
    , (,) "l5" $ \sexps -> do
        case project sexps :: Either Text [L5] of
          Left err -> Text.IO.putStrLn err
          Right res -> forM_ res (putDocLn . pretty . embed)
    , (,) "l6" $ \sexps -> do
        case project sexps :: Either Text [L6] of
          Left err -> Text.IO.putStrLn err
          Right res -> forM_ res (putDocLn . pretty . embed)
    , (,) "l7" $ \sexps -> do
        case project sexps :: Either Text [L6] of
          Left err -> Text.IO.putStrLn err
          Right res -> Text.IO.putStrLn $ compileObjProg res
    ]

