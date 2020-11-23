
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module OldMain where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Bifunctor (first)
import           Control.Lens.Action
import           Control.Monad
import           Control.Monad.Except
import           Data.Function             ((&))
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Kind
import           Data.Loc
import           Data.Proxy                (Proxy (..))
import           Data.String.Conversions   (convertString)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import           Data.Text.Prettyprint.Doc
import           GHC.TypeLits              (Symbol)
import           System.Environment        (getArgs)
import           System.Exit
import           Type.OpenADT
import           Uft.Pretty
import           Uft.Parse
import           Uft.VScheme.Types
import           Uft.UScheme.Types
import           Uft.UScheme.KNormalize
import           Uft.KNormal.Types
import           Uft.KNormal.ClosureConvert
import           Uft.SExpr.Types
import           Uft.SExpr.Parse
-- import           Uft.Scheme.ConvertPrim
-- import           Uft.Scheme.Disambiguate
-- import           Uft.Scheme.LetStarElim
-- import           Uft.Scheme.ListExpand
-- import           Uft.Scheme.Parse

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
    pipeline <-
        case HashMap.lookup pipelineName pipelines of
          Nothing -> usage
          Just x  -> pure x
    fileContent :: Text <- Text.IO.readFile fileName
    sexps <- case parseSExprs fileName fileContent of
               Left err -> Text.IO.putStrLn err >> exitFailure
               Right sexps -> pure sexps
    res <- runExceptT (pipeline sexps)
    case res of
      Left err -> Text.IO.putStrLn err
      Right () -> pure ()

type Pipeline =
    forall m. (MonadIO m, MonadError Text m)
  => [SExpr]
  -> m ()

pipelines :: (MonadIO m, MonadError Text m)
          => HashMap Text ([SExpr] -> m ())
pipelines = HashMap.fromList
    [ ("vs-vs", schemeToScheme)
    , ("ho-cl", hoToCl)
    -- , ("vs-unamb", schemeToUnamb)
    ]

hoToCl :: Pipeline
hoToCl sexps = do
    liftIO $ print sexps
    input <- case traverse (parseFEither @UScheme PCTop) sexps of
               Left err -> throwError (Text.pack err)
               Right x -> pure x
    liftIO . print $ vsep (map prettyF input)
    output <- runReaderT (traverse closeExp input) mempty
    liftIO . print $ vsep (map prettyF output)

hoToKn :: Pipeline
hoToKn sexps = do
    pure ()
    -- liftIO $ print sexps
    -- input <- case traverse (parseFEither @UScheme PCTop) sexps of
               -- Left err -> throwError (Text.pack err)
               -- Right x -> pure x
    -- liftIO . print $ vsep (map prettyF input)
    -- [output] <- runReaderT (traverse closeExp input) mempty
    -- liftIO . print $ vsep (map prettyF output)
    -- knorm <- knormalize output
    -- liftIO . print $ vsep (map prettyF knorm)

schemeToScheme :: Pipeline
schemeToScheme sexps = do
    scheme <- case traverse (parseFEither @VSchemeR PCTop) sexps of
                Left err -> throwError (Text.pack err)
                Right scheme -> pure scheme
    liftIO . print $ vsep (map prettyF scheme)

-- schemeToUnamb :: Pipeline
-- schemeToUnamb fileName fileContent = do
    -- scheme <- parseScheme fileName fileContent
    -- unamb <- scheme ^! to listExpand
                     -- . to letStarElim
                     -- . act convertPrim
                     -- . act disambiguate
    -- liftIO . print . prettyF $ unamb

-- schemeToUnamb :: Pipeline
-- schemeToUnamb fileName fileContent = do
    -- scheme <- parseScheme @(StmtRows (ExpRows LitRows)) @(ExpRows LitRows) @LitRows
        -- fileName fileContent
    -- liftIO $ sequence_ $ map (print . prettyF) scheme

-- fileToScheme :: (FilePath, Text) -> Either Text Scheme.Prog
-- fileToScheme = transform @"parseScheme"

-- fileToUnamb :: (FilePath, Text) -> Either Text Unamb.Prog
-- fileToUnamb = fileToScheme >=> transform @"disambiguate"

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
    asm <- liftEither $ Asm.parseAsm fileName fileContent
    vo <- Asm.toVO asm
    liftIO $ Text.IO.putStr vo
{- 
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
 -}
