
module Language.Scheme.L1.Parse
    ( 
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logic
import           Control.Monad.Trans
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Bifunctor                   (first)
import           Data.Deriving
import           Data.Foldable
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.HashSet                     (HashSet)
import qualified Data.HashSet                     as HashSet
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Language.Scheme.L0.Ast
import           Language.Scheme.L1.Ast
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Debug.Trace
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Util

newtype M a = M { unM :: StateT (Env, Int) (ExceptT Text IO) a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

type Env = HashMap Name Denotation

data Denotation
    = DLocal Name
    | DGlobal

freshId :: M Int
freshId = M $ do
    (env, n) <- get
    put (env, n + 1)
    pure n

gensym :: Name -> M Name
gensym (N base _) = do
    n <- freshId
    pure $ N base (Just n)

getEnv :: M Env
getEnv = M $ gets fst

raiseError :: Text -> M a
raiseError = M . lift . throwE

--

disambiguate
    :: OpenADT L0
    -> M (OpenADT L1)
disambiguate = cata alg where
    alg :: Sum L0 (M (OpenADT L1)) -> M (OpenADT L1)
    alg e = do
        env <- getEnv
        case decompose' @L1Same e of
          _ -> raiseError "nyi"

