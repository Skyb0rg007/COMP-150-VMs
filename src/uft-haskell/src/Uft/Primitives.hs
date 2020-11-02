{-# OPTIONS_GHC -Wall #-}
{-
   Module:      Uft.Primitives
   Description: Primitives used by the Uft
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This implementation copies the provided primitives.sml implementation.
   I tried a dependently-typed version, but it doesn't add much since
   my nanopass architecture is already so dynamic
-}

module Uft.Primitives
    ( module Uft.Primitives
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map

data PrimKind = SetsRegister | HasEffect
    deriving (Show, Eq, Ord, Read)

data Prim = Prim
    { _prim_kind  :: !PrimKind
    , _prim_name  :: !Text
    , _prim_arity :: !Int
    }
    deriving (Show, Eq, Ord, Read)

parsePrim :: Text -> Maybe Prim
parsePrim = flip Map.lookup m
    where
    m :: Map Text Prim
    m = Map.fromList $ map (\(x, k, n) -> (x, Prim k x n)) prims
    prims :: [(Text, PrimKind, Int)]
    prims =
        [ (,,) "abs"          SetsRegister 1
        , (,,) "abs"          SetsRegister 1
        , (,,) "/"            SetsRegister 2
        , (,,) "//"           SetsRegister 2
        , (,,) "-"            SetsRegister 2
        , (,,) "+"            SetsRegister 2
        , (,,) "cons"         SetsRegister 2
        , (,,) "copyreg"      SetsRegister 1
        , (,,) "getglobal"    SetsRegister 1
        , (,,) "hash"         SetsRegister 1
        , (,,) "loadliteral"  SetsRegister 1
        , (,,) "check_assert" HasEffect    2
        , (,,) "check"        HasEffect    2
        , (,,) "expect"       HasEffect    2
        , (,,) "println"      HasEffect    1
        , (,,) "print"        HasEffect    1
        , (,,) "printu"       HasEffect    1
        , (,,) "setglobal"    HasEffect    2
        ]

prim :: Text -> Prim
prim x = case parsePrim x of
           Nothing -> error $ "Uft.Primitives.prim: unknown primitive \"" ++ Text.unpack x ++ "\""
           Just p -> p


