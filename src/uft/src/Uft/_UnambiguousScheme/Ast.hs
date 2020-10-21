
{-# LANGUAGE TemplateHaskell #-}

module Uft.UnambiguousScheme.Ast
    ( 
    ) where

import           Data.Maybe      (isJust)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Primitives
import           Uft.Util
