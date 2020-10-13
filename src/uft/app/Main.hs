{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Control.Monad.Except
import           Data.ByteString.Lazy.Lens
import           Data.Loc
import           Data.String.Conversions   (convertString)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import           Data.Text.Prettyprint.Doc
import           System.Environment
import           Uft.Asm.Ast
import           Uft.Asm.LabelElim
import           Uft.Asm.Parse
import           Uft.Asm.ToVO
import           Uft.Scheme.Ast
import           Uft.Scheme.Parse

main :: IO ()
main = asmMain

asmMain :: IO ()
asmMain = do
    input:_ <- getArgs
    case parseAsm "<cmdline>" (Text.pack input) of
      Left err -> Text.IO.putStrLn err
      Right ast -> do
          case labelElim ast of
            Left err -> print err
            Right ast' ->
                case toVO ast' of
                  Left err -> print err
                  Right vo -> Text.IO.putStrLn vo

schemeMain :: IO ()
schemeMain = do
    input:_ <- getArgs
    case parseScheme "" (Text.pack input) of
      Left err -> Text.IO.putStrLn err
      Right prog -> print $ pretty prog
