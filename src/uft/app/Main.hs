
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Data.Loc
import           Data.String.Conversions         (convertString)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.IO                    as Text.IO
import           Data.Text.Prettyprint.Doc
import           System.Environment              (getArgs)
import           System.Exit
import qualified Uft.Asm.Ast                     as Asm
import qualified Uft.Asm.LabelElim               as Asm
import qualified Uft.Asm.Parse                   as Asm
import qualified Uft.Asm.ToVO                    as Asm
import qualified Uft.KNormal.Ast                 as KNormal
import qualified Uft.KNormal.ToScheme            as KNormal
import qualified Uft.KNormal.ToVO                as KNormal
import qualified Uft.Scheme.Ast                  as Scheme
import qualified Uft.Scheme.Disambiguate         as Scheme
import qualified Uft.Scheme.Parse                as Scheme
import qualified Uft.UnambiguousScheme.Ast       as Unamb
import qualified Uft.UnambiguousScheme.ToKNormal as Unamb
import           Uft.Util                        (foldMapM)

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        putStrLn "Usage: uft <file>"
        exitFailure
    let fileName = head args
    fileContent <- Text.IO.readFile fileName
    res <- runExceptT $ pipeline fileName fileContent
    case res of
      Left err -> Text.IO.putStrLn err
      Right () -> pure ()

pipeline
    :: (MonadIO m, MonadError Text m)
    => FilePath
    -> Text
    -> m ()
pipeline fileName fileContent = do
    scheme  :: Scheme.Prog        <- Scheme.parseScheme fileName fileContent
    -- liftIO $ print $ pretty scheme
    unamb   :: Unamb.Prog         <- Scheme.disambiguate scheme
    -- liftIO $ print $ pretty unamb
    knorms  :: [KNormal.Exp Text] <- Unamb.unambToKNorm unamb
    asm     :: Asm.Prog           <- foldMapM KNormal.knormToAsm knorms
    liftIO $ print asm
    -- let scheme' :: Scheme.Prog = KNormal.knormToScheme knorm
    -- liftIO $ print $ pretty scheme'

-- asmMain :: IO ()
-- asmMain = do
    -- input:_ <- getArgs
    -- case parseAsm "<cmdline>" (Text.pack input) of
      -- Left err -> Text.IO.putStrLn err
      -- Right ast -> do
          -- case labelElim ast of
            -- Left err -> print err
            -- Right ast' ->
                -- case toVO ast' of
                  -- Left err -> print err
                  -- Right vo -> Text.IO.putStrLn vo

-- schemeMain :: IO ()
-- schemeMain = do
    -- input:_ <- getArgs
    -- case parseScheme "" (Text.pack input) of
      -- Left err -> Text.IO.putStrLn err
      -- Right prog -> print $ pretty prog
