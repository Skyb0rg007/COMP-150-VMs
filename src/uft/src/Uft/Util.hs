
module Uft.Util
    ( 
    -- * Pretty printing
      prettyText
    , prettyLText
    -- * Text utilities
    , tshow
    -- * Miscellaneous
    , foldMapM
    ) where

import           Data.Foldable                         (foldlM)
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.Lazy                        as Lazy (Text)
import qualified Data.Text.Lazy                        as Lazy.Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import           Data.Text.Prettyprint.Doc             (Pretty (pretty))
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text

prettyText :: Pretty a => a -> Text
prettyText = Pretty.Text.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

prettyLText :: Pretty a => a -> Lazy.Text
prettyLText = Pretty.Text.renderLazy . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Monadic version of 'foldMap'
-- Implemented using a strict left fold
foldMapM :: (Monad m, Monoid w, Foldable t)
         => (a -> m w)
         -> t a
         -> m w
foldMapM f = foldlM go mempty
    where go acc a = f a >>= \w -> pure $! acc <> w



