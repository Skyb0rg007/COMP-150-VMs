{-# LANGUAGE TemplateHaskell #-}
module Language.Scheme.L1.Ast
    ( module Language.Scheme.L1.Ast
    , module Language.Scheme.L0.Ast
    ) where

import           Control.DeepSeq        (NFData)
import           Data.Char              (chr, isAlphaNum, isPrint, ord)
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Kind              (Type)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import           Data.Monoid            (Endo (Endo, appEndo))
import           Data.String            (IsString (fromString))
import           Data.Text              (Text)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Word              (Word8)
import           GHC.Generics           (Generic)
import           Language.Scheme.L0.Ast
import qualified Language.Scheme.L0.Ast as L0
import           Numeric                (showGFloat, showIntAtBase)
import           Text.Read              (Read (readPrec))
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Util

-- L1: Post-macro expansion

type L1 =
    -- Literals
    '[ L0.CharF
     , L0.StringF
     , L0.SymbolF
     , L0.BoolF
     , L0.NumF
     , L0.ByteVectorF
     , L0.EmptyF
     , L0.PairF
     , L0.VectorF

    -- Expressions
     , VarLocalF
     , VarGlobalF
     , LambdaF
     , LetF
     , LetStarF
     , LetRecF
     , LetRecStarF
     , BeginF
     , SetLocalF
     , SetGlobalF
     , ApplyF

     , DefineF
     , ValF
     ]

newtype VarLocalF (a :: Type) = VarLocalF' Name
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

newtype VarGlobalF (a :: Type) = VarGlobalF' Text
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data LambdaF a = LambdaF' [Name] [a]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data LetF a = LetF' [(Name, a)] [a]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data LetStarF a = LetStarF' [(Name, a)] [a]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data LetRecF a = LetRecF' [(Name, a)] [a]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data LetRecStarF a = LetRecStarF' [(Name, a)] [a]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

newtype BeginF a = BeginF' [a]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data SetLocalF a = SetLocalF' Name a
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data SetGlobalF a = SetGlobalF' Text a
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data ApplyF a = ApplyF' a [a]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data DefineF a = DefineF' Name [Name] [a]
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data ValF a = ValF' Name a
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

-- * Deriving stuff
derive [deriveOpenADT, deriveShow1, deriveEq1, deriveOrd1, deriveRead1]
    [ ''VarLocalF
    , ''VarGlobalF 
    , ''LambdaF 
    , ''LetF
    , ''LetStarF
    , ''LetRecF
    , ''LetRecStarF
    , ''BeginF 
    , ''SetLocalF 
    , ''SetGlobalF 
    , ''ApplyF 
    , ''DefineF 
    , ''ValF
    ]

-- instance PrettyF SymbolF where
    -- prettyF' (SymbolF' n) = prettyF' (L0.SymbolF' (Text.pack (show n)))

