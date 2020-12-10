{-# OPTIONS_GHC -Wall #-}
{-
   Module:      Language.Scheme.Primitives
   Description: Primitives used by the Uft
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This implementation copies the provided primitives.sml implementation.
   I tried a dependently-typed version, but it doesn't add much
-}

module Language.Scheme.Primitives
    ( PrimKind (..)
    , Prim (..)
    , _prim_inputArity
    , parsePrim
    , prim
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as Text

data PrimKind = SetsRegister | HasEffect
    deriving (Show, Eq, Ord, Read)

data Prim = Prim
    { _prim_name      :: !Text -- ^ The name used to lookup the primitive
    , _prim_voName    :: !Text -- ^ The name used to print the primitive
    , _prim_arity     :: !Int  -- ^ The primitive's arity (not including literal argument!)
    , _prim_litarg    :: !Bool -- ^ Whether the primitive takes a literal argument
    , _prim_destreg   :: !Bool -- ^ Whether the primitive's first arg is the destination register
    , _prim_effectful :: !Bool -- ^ Whether the primitive can be optimized out
    , _prim_visible   :: !Bool -- ^ Whether the primitive can be called in user code
    }

-- | Number of input arguments
_prim_inputArity :: Prim -> Int
_prim_inputArity p =
    if _prim_destreg p
       then _prim_arity p - 1
       else _prim_arity p

-- | Parse the primitive from user code (hides non-visible primitives)
parsePrim :: Text -> Maybe Prim
parsePrim x =
    case HashMap.lookup x prims of
      Just p
        | _prim_visible p -> Just p
      _ -> Nothing

prims :: HashMap Text Prim
prims = HashMap.fromList (map (\p -> (_prim_name p, p)) m) where
    m :: [Prim]
    m =
        -- Control flow
        --     name            voName           arity litarg destreg effectful visible
        [ Prim "copyreg"       "copyreg"        2     False  True    False     False
        , Prim "halt"          "halt"           0     False  False   True      False
        , Prim "call"          "call"           3     False  True    True      False
        , Prim "tailcall"      "tailcall"       2     False  False   True      False
        , Prim "return"        "return"         1     False  False   True      False
        , Prim "loadliteral"   "loadliteral"    1     True   True    False     False
        , Prim "goto"          "goto"           1     False  False   True      False
        , Prim "if"            "if"             1     False  False   True      False

        -- Math
        --     name            voName           arity litarg destreg effectful visible
        , Prim "+"             "add"            3     False  True    False     True
        , Prim "/"             "div"            3     False  True    False     True
        , Prim "//"            "idiv"           3     False  True    False     True
        , Prim "-"             "sub"            3     False  True    False     True
        , Prim "*"             "mul"            3     False  True    False     True
        , Prim "abs"           "abs"            2     False  True    False     True

        -- Misc
        --     name            voName           arity litarg destreg effectful visible
        , Prim "hash"          "hash"           2     False  True    False     True
        , Prim "print"         "print"          1     False  False   True      True
        , Prim "printu"        "printu"         1     False  False   True      True
        , Prim "println"       "println"        1     False  False   True      True

        -- Globals
        --     name            voName           arity litarg destreg effectful visible
        , Prim "get-global"    "getglobal"      1     True   True    False     False
        , Prim "set-global!"   "setglobal"      1     True   False   True      False

        -- Testing
        --     name            voName           arity litarg destreg effectful visible
        , Prim "check"         "check"          1     True   False   True      False
        , Prim "expect"        "expect"         1     True   False   True      False
        , Prim "check-assert"  "checkassert"    1     True   False   True      False

        -- Closures
        --     name            voName           arity litarg destreg effectful visible
        , Prim "closure"       "closure"        3     False  True    False     False
        , Prim "getclslot"     "getclslot"      3     False  True    False     False
        , Prim "setclslot"     "setclslot"      3     False  False   True      False

        -- Boxes
        --     name            voName           arity litarg destreg effectful visible
        , Prim "box?"          "boxq"           2     False  True    False     True
        , Prim "box"           "box"            2     False  True    False     True
        , Prim "unbox"         "unbox"          2     False  True    False     True
        , Prim "set-box!"      "set_box"        3     False  False   True      True

        -- Void
        --     name            voName           arity litarg destreg effectful visible
        , Prim "void"          "void"           1     False  True    False     True

        -- Pairs
        --     name            voName           arity litarg destreg effectful visible
        , Prim "pair?"         "pairq"          2     False  True    False     True
        , Prim "cons"          "cons"           3     False  True    False     True
        , Prim "car"           "car"            2     False  True    False     True
        , Prim "cdr"           "cdr"            2     False  True    False     True
        , Prim "set-car!"      "set_car"        2     False  False   True      True
        , Prim "set-cdr!"      "set_cdr"        2     False  False   True      True

        -- , Prim "list->vector"  "list_to_vector" 2     False  True    False     True
        -- , Prim "append"        "append"         3     False  True    False     True
        ]

prim :: Text -> Prim
prim p = fromMaybe (error msg) (HashMap.lookup p prims)
    where msg = "Uft.Primitives.prim: unknown primitive \"" ++ Text.unpack p ++ "\""

