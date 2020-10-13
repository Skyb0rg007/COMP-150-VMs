
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Uft.Scheme.Prims
    ( Prim (..)
    , primName
    , primParse
    , primArity
    ) where

import           Data.Bimap                (Bimap)
import qualified Data.Bimap                as Bimap
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty (pretty))

data Prim
    = PrimCons
    | PrimSetGlobal
    | PrimGetGlobal
    | PrimCheck
    | PrimExpect
    | PrimCheckAssert
    | PrimLoadLiteral
    deriving (Show, Eq, Ord)

primArity :: Prim -> Int
primArity = \case
    PrimCons        -> 2
    PrimSetGlobal   -> 2
    PrimGetGlobal   -> 2
    PrimCheck       -> 2
    PrimExpect      -> 2
    PrimCheckAssert -> 2
    PrimLoadLiteral -> 2

primNameMap :: Bimap Prim Text
primNameMap = Bimap.fromList
    [ (,) PrimCons        "cons"
    , (,) PrimSetGlobal   "setglobal"
    , (,) PrimGetGlobal   "getglobal"
    , (,) PrimCheck       "check"
    , (,) PrimExpect      "expect"
    , (,) PrimCheckAssert "check-assert"
    , (,) PrimLoadLiteral "loadliteral"
    ]

primName :: Prim -> Text
primName p = primNameMap Bimap.! p

primParse :: Text -> Maybe Prim
primParse p = Bimap.lookupR p primNameMap

instance Pretty Prim where
    pretty p = pretty $ primName p
