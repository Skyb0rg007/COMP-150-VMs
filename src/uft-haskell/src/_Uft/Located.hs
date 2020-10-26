
{-# LANGUAGE TemplateHaskell #-}

module Uft.Located
    ( LocF (LocF')
    , pattern LocF
    , pattern Loc
    , LocRowF
    , unLocF
    ) where

import           Data.Deriving
import           Data.Functor.Classes
import           Data.Loc             (Located (..), SrcLoc (..))
import           Data.Row             (type (.==))
import           Data.Text            (Text)
import           Text.Read            (Read (readPrec))
import           Type.OpenADT.TH
import           Uft.Pretty

openADT id [d|
    data LocF a = LocF' SrcLoc a
    |]
openADTDerive1 ''LocF

type LocRowF = ("loc" .== LocF)

unLocF :: LocF a -> a
unLocF (LocF' _ a) = a

-- Same as 'L'
instance Located (LocF a) where
    locOf (LocF' (SrcLoc loc) _) = loc

-- Ignore LocF when pretty-printing
instance PrettyF LocF where
    prettyF' (LocF' _ a) = a

