
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Uft.Scheme.Unambiguous
    ( Stmt (..)
    , Exp (..)
    , ExpF (..)
    , LetKind (..)
    , Literal (..)
    , Prim (..)
    ) where

import           Control.Monad.Except
import           Data.Foldable            (traverse_)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import           Data.Maybe               (isJust)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Vector              (Vector)
import qualified Data.Vector              as Vector

data Stmt
    = Val !Text !Exp
    | Define !Text !(Vector Text) !Exp
    | Exp !Exp
    | CheckExpect !(Text, Exp) !(Text, Exp)
    | CheckAssert !(Text, Exp)
    deriving (Show, Eq, Ord)

data Exp
    = ExpLit !Literal
    | ExpLocalVar !Text
    | ExpGlobalVar !Text
    | ExpSetLocal !Text !Exp
    | ExpSetGlobal !Text !Exp
    | ExpIf !Exp !Exp !Exp
    | ExpWhile !Exp !Exp
    | ExpBegin !(Vector Exp)
    | ExpFunApply !Exp !(Vector Exp)
    | ExpPrimApply !Prim !(Vector Exp)
    | ExpLet !LetKind !(Vector (Text, Exp)) !Exp
    | ExpLambda !(Vector Text) !Exp
    deriving (Show, Eq, Ord)

data LetKind = Let | LetRec
    deriving (Show, Eq, Ord)

data Literal
    = LitSym !Text
    | LitNum !Double
    | LitBool !Bool
    | LitEmpty
    deriving (Show, Eq, Ord)

data Prim
    = PrimCons
    deriving (Show, Eq, Ord)

makeBaseFunctor ''Exp

