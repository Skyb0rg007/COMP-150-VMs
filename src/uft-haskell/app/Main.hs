
module Main
    ( main
    ) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as Text.IO
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Language.Scheme.SExp.Ast
import           Language.Scheme.SExp.Parse
import           Language.Scheme.SExp.Class
import           Language.Scheme.L0.Ast
-- import           Language.Scheme.L1.MacroExpand
import           System.Environment
import           System.Exit

putDocLn :: Doc ann -> IO ()
putDocLn x = putDoc x >> putStrLn ""

usage :: IO a
usage = do
    putStrLn "Usage: uft <in>-<out> <file>"
    exitFailure

main :: IO ()
main = do
    args :: [String] <- getArgs
    (pipelineName :: Text, fileName :: FilePath) <-
        case args of
          [a, b] -> pure (Text.pack a, b)
          _ -> usage
    when (pipelineName /= "l0-l0")
        usage
    fileContent <- Text.IO.readFile fileName
    case parseSExp fileName fileContent of
      Left err  -> putStrLn err
      Right res -> do
          forM_ res (putDocLn . pretty)
          case project res :: Either Text [L0] of
            Left err   -> Text.IO.putStrLn err
            Right res' -> do
                forM_ res' (putDocLn . pretty . embed)
