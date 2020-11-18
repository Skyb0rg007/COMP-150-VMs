
-- Input language

module Uft.Language.L0
    ( module Uft.Language.L0
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Kind           (Type)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Word           (Word8)
import           Type.OpenADT
import           Uft.Naming
import           Uft.Parse
import           Uft.Pretty
import           Uft.Primitives
import           Uft.SExpr.Types
import           Uft.Util

-- * Datums
-- ** Self-evaluating literals

newtype NumF (a :: Type) = LNumF' Double

newtype SymbolF (a :: Type) = SymbolF' Name

newtype StringF (a :: Type) = StringF' Text

newtype CharF (a :: Type) = CharF' Char

newtype BoolF (a :: Type) = BoolF' Bool

newtype ByteVectorF (a :: Type) = ByteVectorF' [Word8]

-- ** Quoted-only literals

data EmptyF (a :: Type) = EmptyF'

data PairF a = PairF' a a

newtype VectorF a = VectorF' [a]

-- * Expressions
-- ** Primitive Expressions

data VarF (a :: Type) = VarF' Name

newtype QuoteF a = QuoteF' a

data ApplyF a = ApplyF' a [a]

data LambdaF a = LambdaF' [Name] [a]

data IfF a = IfF' a a (Maybe a)

data SetF a = SetF' Name a

data IncludeF (a :: Type) = IncludeF' [Text]

-- * Quasiquotation

newtype QuasiquoteF a = QuasiquoteF' a

newtype UnquoteF a = UnquoteF' a

newtype UnquoteSplicingF a = UnquoteSplicingF' a

-- * Derived expresions

newtype BeginF a = BeginF' [a]

data LetF a = LetF' [(Name, a)] [a]

data LetRecF a = LetRecF' [(Name, a)] [a]

data WhileF a = WhileF' a a

