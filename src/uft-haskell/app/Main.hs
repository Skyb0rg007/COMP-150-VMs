
module Main
    ( main
    ) where

import           Control.Monad
import           Data.Foldable
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as Text.IO
import           Data.Text.Prettyprint.Doc.Render.Text
import           Language.Scheme.L0.Ast
import           Language.Scheme.L0.Parse
import           Language.Scheme.L1.Ast
import           Language.Scheme.L1.MacroExpand
import           System.Environment
import           System.Exit

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
    case parseL0 fileName fileContent of
      Left err   -> Text.IO.putStrLn err
      Right res -> do
          forM_ res $ \x -> putDoc (prettyL0 x) >> putStrLn ""
          res' <- runM $ traverse (macroExpand defaultEnv) res
          case res' of
            Left err -> Text.IO.putStrLn err
            Right x -> forM_ x $ \x -> putDoc (prettyL0 x) >> putStrLn ""
