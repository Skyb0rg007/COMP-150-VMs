{-# OPTIONS_GHC -Wall #-}

module Language.Scheme.SExp.Class
    ( Project (..)
    , Embed (..)
    ) where

import           Data.Text                (Text)
import           Language.Scheme.SExp.Ast (SExp)

class Project x where
    project :: [SExp] -> Either Text [x]

instance Project SExp where
    project = pure

class Embed x where
    embed :: x -> SExp

instance Embed SExp where
    embed = id

