{-# LANGUAGE StrictData, TemplateHaskell #-}

module Uft.KNormal.Types
    ( module Uft.KNormal.Types
    , module Uft.UScheme.Types
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
import           Uft.UScheme.Types

type KNormal n =
    '[ LBoolF
     , LNumF
     , LEmptyF
     , LSymF
     , ESeqF
     , EVarLocalF n
     , ESetLocalF n
     , KNIfF n
     , KNLetF n
     , KNLetRecF n
     , KNWhileF n
     , KNCapturedF
     , KNClosureF n
     , KNApplyF n
     , KNPrimAppF n
     ]

-- | KNormal primitive application
-- Maybe includes a literal
data KNPrimAppF n a = KNPrimAppF' Prim [n] (Maybe a)
    deriving (Functor, Foldable, Traversable)

instance PrettyF (KNPrimAppF n) where
    prettyF' (KNPrimAppF' p args mLit) =
        "<knprimapp>"

data KNApplyF n (a :: Type) = KNApplyF' n [n]
    deriving (Functor, Foldable, Traversable)

instance Pretty n => PrettyF (KNApplyF n) where
    prettyF' _ = "<knapply>"

data ESeqF a = ESeqF' a a
    deriving (Functor, Foldable, Traversable)

instance PrettyF ESeqF where
    prettyF' _ = "<eseq>"

data KNIfF n a = KNIfF' n a a
    deriving (Functor, Foldable, Traversable)

instance PrettyF (KNIfF n) where
    prettyF' _ = "<knif>"

data KNLetF n a = KNLetF' n a a
    deriving (Functor, Foldable, Traversable)

data KNLetRecF n a = KNLetRecF' [(n, KNClosureF n a)] a
    deriving (Functor, Foldable, Traversable)

instance PrettyF (KNLetRecF n) where
    prettyF' _ = "<knletrec>"

data KNWhileF n a = KNWhileF' n a a
    deriving (Functor, Foldable, Traversable)

data KNLambdaF n a = KNLambdaF' [n] a
    deriving (Functor, Foldable, Traversable)

instance Pretty n => PrettyF (KNLambdaF n) where
    prettyF' (KNLambdaF' args body) = parens $
        "lambda"
        <+> parens (hsep (map pretty args))
        <+> body

data KNCapturedF (a :: Type) = KNCapturedF' Int
    deriving (Functor, Foldable, Traversable)

instance PrettyF KNCapturedF where
    prettyF' (KNCapturedF' n) = parens $
        "captured" <+> pretty n

data KNClosureF n a = KNClosureF' (KNLambdaF n a) [a]
    deriving (Functor, Foldable, Traversable)

instance Pretty n => PrettyF (KNClosureF n) where
    prettyF' (KNClosureF' lam closed) = parens $
        "closure" <+> prettyF' lam <+> hsep closed

derive [deriveOpenADT, deriveShow1, deriveEq1, deriveOrd1, deriveRead1]
    [ ''KNPrimAppF
    , ''KNApplyF
    , ''ESeqF
    , ''KNLetF
    , ''KNLetRecF
    , ''KNWhileF
    , ''KNLambdaF
    , ''KNCapturedF
    , ''KNClosureF
    , ''KNIfF
    ]

