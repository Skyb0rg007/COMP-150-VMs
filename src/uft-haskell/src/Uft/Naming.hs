{-
   Module:      Uft.Naming
   Description: Definition of Names
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   Used to implement variable names
-}
module Uft.Naming
    ( module Uft.Naming
    ) where

import           Control.Monad.State.Strict
import           Control.DeepSeq (NFData)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           GHC.Generics    (Generic)
import           Data.String     (IsString (fromString))
import           Data.Hashable

-- | Named references
-- The integer is used when renaming
data Name = N Text (Maybe Int)
    deriving (Eq, Ord, Generic, NFData, Hashable)

instance Show Name where
    show (N n Nothing) = Text.unpack n
    show (N n (Just m)) = Text.unpack n ++ "#" ++ show m

instance IsString Name where
    fromString = Name . fromString

pattern Name :: Text -> Name
pattern Name n = N n Nothing

freshName :: MonadState Int m => Text -> m Name
freshName base = do
    n <- get
    put (n + 1)
    pure $ N base (Just n)

