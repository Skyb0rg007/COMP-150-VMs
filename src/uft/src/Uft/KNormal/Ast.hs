{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Uft.KNormal.Ast
    ( Exp (..)
    , Literal (..)
    ) where

import           Control.Monad.Except
import           Data.Foldable             (traverse_)
import           Data.Foldable             (toList)
import           Data.Functor.Foldable.TH  (makeBaseFunctor)
import           Data.Maybe                (isJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Uft.Asm.Ast               (Cmd, LitCmd)
import           Uft.Scheme.Prims          (Prim (..))

data Exp a
    = ExpLit !Literal
    | ExpVar !a
    | ExpIf !(Exp a) !(Exp a) !(Exp a)
    | ExpLet !a !(Exp a) !(Exp a)
    | ExpSeq !(Exp a) !(Exp a)
    | ExpSet !a !(Exp a)
    | ExpWhile !Text !(Exp a) !(Exp a)
    | ExpFunCode !(Vector a) !(Exp a)
    | ExpFunCall !a !(Vector a)
    | ExpCmd !Prim !(Vector a)
    | ExpLitCmd !Prim !(Vector a) !Literal
    deriving (Show, Eq, Ord)

data Literal
    = LitNum !Double
    | LitBool !Bool
    | LitSym !Text
    | LitEmpty
    | LitNil
    deriving (Show, Eq, Ord)


