{-# LANGUAGE StrictData, TemplateHaskell #-}

module Uft.UScheme.Types
    ( module Uft.UScheme.Types
    , module Uft.VScheme.Types
    ) where

import           Data.Deriving
import           Data.Kind       (Type)
import           Data.Maybe      (catMaybes)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Word       (Word8)
import           Debug.Trace
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Parse
import           Uft.Pretty
import           Uft.SExpr.Types
import           Uft.Util
import           Uft.Primitives
import           Uft.VScheme.Types

-- | Variants that are part of the UScheme language datatype
-- Note that the order matters for parsing
type UScheme =
    -- Literals
    '[ LBoolF
     -- , LByteVecF
     -- , LCharF
     , LNumF
     -- , LStrF
     , LEmptyF
     , LSymF
    -- Definitions
     , DValF
     , DDefineF
     , DCheckExpectF
     , DCheckAssertF
    -- Expressions
     , EBeginF
     , ELambdaF
     , ESetGlobalF
     , ESetLocalF Text
     , EIfF
     , ELetF
     , ELetRecF
     , EWhileF
     , EVarGlobalF
     , EApplyF
     , EVarLocalF Text
     , EPrimApplyF -- Note that this cannot be parsed
     ]

-- | Local variable
newtype EVarLocalF n (a :: Type) = EVarLocalF' n
    deriving (Functor, Foldable, Traversable)

instance Pretty n => PrettyF (EVarLocalF n) where
    prettyF' (EVarLocalF' n) = styleVar (pretty n)

instance ParseF (EVarLocalF Text) where
    parseF' PCNotQ (SAtom (SSymbol x)) =
        pure $ EVarLocalF' x
    parseF' _ _ = empty

-- | Local set
data ESetLocalF n a = ESetLocalF' n a
    deriving (Functor, Foldable, Traversable)

instance Pretty n => PrettyF (ESetLocalF n) where
    prettyF' (ESetLocalF' x e) = parens $
        "set!" <+> pretty x <+> e

instance ParseF (ESetLocalF Text) where
    parseF' PCNotQ (SList ["set", SAtom (SSymbol x), e]) =
        pure $ ESetLocalF' x (PCExp, e)
    parseF' _ _ = empty

-- | Global variable
newtype EVarGlobalF (a :: Type) = EVarGlobalF' Text
    deriving (Functor, Foldable, Traversable)

instance PrettyF EVarGlobalF where
    prettyF' (EVarGlobalF' n) = styleVar (pretty n)

instance ParseF EVarGlobalF where
    parseF' PCNotQ (SList ["global", SAtom (SSymbol x)]) =
        pure $ EVarGlobalF' x
    parseF' _ _ = empty

-- | Global set
data ESetGlobalF a = ESetGlobalF' Text a
    deriving (Functor, Foldable, Traversable)

instance PrettyF ESetGlobalF where
    prettyF' (ESetGlobalF' x e) = parens $
        "set!" <+> pretty x <+> e

instance ParseF ESetGlobalF where
    parseF' PCNotQ (SList ["set", SList ["global", SAtom (SSymbol x)], e]) =
        pure $ ESetGlobalF' x (PCExp, e)
    parseF' _ _ = empty

derive [deriveOpenADT, deriveShow1, deriveEq1, deriveOrd1, deriveRead1]
    [ ''EVarLocalF
    , ''ESetLocalF
    , ''EVarGlobalF
    , ''ESetGlobalF
    ]
