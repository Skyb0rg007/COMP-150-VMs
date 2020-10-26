
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

-- import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Data.Kind
import           Data.Loc
import           Data.Proxy                   (Proxy (..))
import           Data.Singletons.Prelude.List
import           Data.String.Conversions      (convertString)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text.IO
import           Data.Text.Prettyprint.Doc
import           GHC.TypeLits                 (Symbol)
import           System.Environment           (getArgs)
import           System.Exit
import qualified Uft.Scheme.Ast               as Scheme
import           Uft.Scheme.Disambiguate      ()
import           Uft.Scheme.Parse             ()
import           Uft.Transform
import qualified Uft.UnambiguousScheme.Ast    as Unamb

main = pure ()

fileToScheme :: (FilePath, Text) -> Either Text Scheme.Prog
fileToScheme = transform @"parseScheme"

fileToUnamb :: (FilePath, Text) -> Either Text Unamb.Prog
fileToUnamb = fileToScheme >=> transform @"disambiguate"

{- import qualified Uft.Asm.Ast                     as Asm
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
    -- liftIO $ print $ fmap (pretty . KNormal.knormToScheme) knorms
    asm     :: Asm.Prog           <- foldMapM KNormal.knormToAsm knorms
    vo      :: Text               <- Asm.toVO asm
    liftIO $ Text.IO.putStr vo
    -- let scheme' :: Scheme.Prog = KNormal.knormToScheme knorm
    -- liftIO $ print $ pretty scheme'
 -}
