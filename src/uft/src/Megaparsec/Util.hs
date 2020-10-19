
module Megaparsec.Util
    ( getPos
    , wrapLoc
    ) where

import qualified Text.Megaparsec as M
import           Data.Loc

-- | Access the current location the parser is in
-- Converts from 'M.Pos' to 'Pos'
getPos :: M.MonadParsec e s m => m Pos
getPos = do
    sourcePos <- M.getSourcePos
    offset <- M.getOffset
    let name = M.sourceName sourcePos
        line = M.sourceLine sourcePos
        column = M.sourceColumn sourcePos
    pure $ Pos name (M.unPos line) (M.unPos column) offset

-- | Parses p, but adds location info via the 'L' datatype
wrapLoc :: M.MonadParsec e s m => m a -> m (L a)
wrapLoc p = do
    pos1 <- getPos
    res <- p
    pos2 <- getPos
    pure $ L (Loc pos1 pos2) res


