
module Language.Scheme.Name
    ( Name (..)
    , pattern Name
    , unName
    ) where

import           Control.DeepSeq        (NFData)
import           Data.Char              (chr, isAlphaNum, isPrint, ord)
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Hashable          (Hashable)
import           Data.Kind              (Type)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import           Data.Monoid            (Endo (Endo, appEndo))
import           Data.String            (IsString (fromString))
import           Data.Text              (Text)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Word              (Word8)
import           GHC.Generics           (Generic)
import           Numeric                (showGFloat, showIntAtBase)
import           Text.Read              (Read (readPrec))
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Util

-- * Names
-- These are needed because symbols can be any sequence of characters
-- In addition, the original name of the symbol must be retained for the form (quote x)

data Name = N
    { _name_base :: !Text
    , _name_id   :: !(Maybe Int)
    }
    deriving (Show, Eq, Ord, Read, Generic, NFData, Hashable)

pattern Name :: Text -> Name
pattern Name x <- N x _
    where Name x = N x Nothing
{-# COMPLETE Name #-}

unName :: Name -> Text
unName (N base id) = base <> maybe "" (Text.cons '.' . tshow) id

instance Pretty Name where
    pretty = pretty . unName

instance IsString Name where
    fromString = Name . fromString

