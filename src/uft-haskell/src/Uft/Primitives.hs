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
    , _prim_lit   :: !Bool
    }
    deriving (Show, Eq, Ord, Read)

parsePrim :: Text -> Maybe Prim
parsePrim = flip Map.lookup m
    where
    m :: Map Text Prim
    m = Map.fromList $ map (\(x, k, n, l) -> (x, Prim k x n l)) prims
    prims :: [(Text, PrimKind, Int, Bool)]
    prims =
        [ (,,,) "abs"          SetsRegister 1 False
        , (,,,) "abs"          SetsRegister 1 False
        , (,,,) "/"            SetsRegister 2 False
        , (,,,) "//"           SetsRegister 2 False
        , (,,,) "-"            SetsRegister 2 False
        , (,,,) "+"            SetsRegister 2 False
        , (,,,) "cons"         SetsRegister 2 False
        , (,,,) "copyreg"      SetsRegister 1 False
        , (,,,) "getglobal"    SetsRegister 1 True
        , (,,,) "hash"         SetsRegister 1 False
        , (,,,) "loadliteral"  SetsRegister 1 True
        , (,,,) "check_assert" HasEffect    2 True
        , (,,,) "check"        HasEffect    2 True
        , (,,,) "expect"       HasEffect    2 True
        , (,,,) "println"      HasEffect    1 False
        , (,,,) "print"        HasEffect    1 False
        , (,,,) "printu"       HasEffect    1 False
        , (,,,) "setglobal"    HasEffect    2 True
        , (,,,) "if"           HasEffect    1 False
        , (,,,) "call"         HasEffect    3 False
        ]

prim :: Text -> Prim
prim x = case parsePrim x of
           Nothing -> error $ "Uft.Primitives.prim: unknown primitive \"" ++ Text.unpack x ++ "\""
           Just p -> p


