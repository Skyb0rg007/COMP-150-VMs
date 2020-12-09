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

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as Text

data PrimKind = SetsRegister | HasEffect
    deriving (Show, Eq, Ord, Read)

data Prim = Prim
    { _prim_kind  :: !PrimKind -- ^ Whether the primitive causes side effects
    , _prim_name  :: !Text -- ^ The name used when generating object code
    , _prim_arity :: !Int  -- ^ The number of input arguments
    , _prim_lit   :: !Bool -- ^ Whether or not the primitive takes a literal argument
    }
    deriving (Show, Eq, Ord, Read)

parsePrim :: Text -> Maybe Prim
parsePrim = flip HashMap.lookup prims

prims :: HashMap Text Prim
prims = HashMap.fromList $ map (\(x, k, n, l) -> (x, Prim k x n l)) m
    where
    m :: [(Text, PrimKind, Int, Bool)]
    m = [ (,,,) "abs"          SetsRegister 1 False
        , (,,,) "append"       SetsRegister 2 False
        , (,,,) "list->vector" SetsRegister 1 False
        , (,,,) "/"            SetsRegister 2 False
        , (,,,) "//"           SetsRegister 2 False
        , (,,,) "-"            SetsRegister 2 False
        , (,,,) "+"            SetsRegister 2 False
        , (,,,) "*"            SetsRegister 2 False
        , (,,,) "copyreg"      SetsRegister 1 False
        , (,,,) "getglobal"    SetsRegister 1 True
        , (,,,) "hash"         SetsRegister 1 False
        , (,,,) "loadliteral"  SetsRegister 1 True
        , (,,,) "void"         SetsRegister 1 False
        , (,,,) "check_assert" HasEffect    2 True
        , (,,,) "check"        HasEffect    2 True
        , (,,,) "expect"       HasEffect    2 True
        , (,,,) "println"      HasEffect    1 False
        , (,,,) "print"        HasEffect    1 False
        , (,,,) "printu"       HasEffect    1 False
        , (,,,) "setglobal"    HasEffect    2 True
        , (,,,) "if"           HasEffect    1 False

        , (,,,) "cons"         SetsRegister 2 False
        , (,,,) "car"          SetsRegister 2 False
        , (,,,) "cdr"          SetsRegister 2 False

        , (,,,) "call"         HasEffect    3 False
        , (,,,) "tailcall"     HasEffect    2 False
        , (,,,) "return"       HasEffect    1 False

        , (,,,) "boxq"         SetsRegister 2 False
        , (,,,) "box"          SetsRegister 2 False
        , (,,,) "unbox"        SetsRegister 2 False
        , (,,,) "set_box"      HasEffect    2 False

        , (,,,) "closure"      SetsRegister 3 False
        , (,,,) "getclslot"    SetsRegister 3 False
        , (,,,) "setclslot"    HasEffect    3 False
        ]

prim :: Text -> Prim
prim p = fromMaybe (error msg) $ parsePrim p
    where msg = "Uft.Primitives.prim: unknown primitive \"" ++ Text.unpack p ++ "\""

