
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Uft.Scheme.Ast
    ( Stmt (..)
    , Exp (..)
    , ExpF (..)
    , LetKind (..)
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

data Stmt
    = Val !Text !Exp
    | Define !Text !(Vector Text) !Exp
    | Exp !Exp
    | CheckExpect !Exp !Exp
    | CheckAssert !Exp
    deriving (Show, Eq, Ord)

data Exp
    = ExpLit !Literal
    | ExpVar !Text
    | ExpSet !Text !Exp
    | ExpIf !Exp !Exp !Exp
    | ExpWhile !Exp !Exp
    | ExpBegin !(Vector Exp)
    | ExpApply !Exp !(Vector Exp)
    | ExpLet !LetKind !(Vector (Text, Exp)) !Exp
    | ExpLambda !(Vector Text) !Exp
    deriving (Show, Eq, Ord)

data LetKind = Let | LetRec
    deriving (Show, Eq, Ord)

data Literal
    = LitSym !Text
    | LitNum !Double
    | LitBool !Bool
    | LitPair !Literal !Literal
    | LitEmpty
    deriving (Show, Eq, Ord)

instance Pretty Exp where
    pretty = \case
        ExpLit lit -> pretty lit
        ExpVar x -> pretty x
        ExpSet x e -> "(" <> pretty x <+> pretty e <> ")"
        ExpIf e1 e2 e3 -> "(if" <+> pretty e1 <+> pretty e2 <+> pretty e3 <> ")"
        ExpWhile e1 e2 -> "(while" <+> pretty e1 <+> pretty e2 <> ")"
        ExpBegin es -> "(begin" <> foldMap ((" " <>) . pretty) es <> ")"
        ExpApply f args -> "(" <> pretty f  <> foldMap ((" " <>) . pretty) args <> ")"
        ExpLet kind binds e -> "(" <> pretty kind <+> "(" <> prettyBinds binds <> ")" <+> pretty e <> ")"
            where
                prettyBinds :: Foldable f => f (Text, Exp) -> Doc ann
                prettyBinds = vsep . foldr (\x acc -> prettyBind x : acc) mempty
                prettyBind :: (Text, Exp) -> Doc ann
                prettyBind (x, e) = "[" <> pretty x <+> pretty e <> "]"
        ExpLambda args e -> "(lambda" <+> "(" <> hsep (toList (fmap pretty args)) <> ")" <+> pretty e <> "))"

instance Pretty LetKind where
    pretty Let = "let"
    pretty LetRec = "letrec"

instance Pretty Literal where
    pretty = \case
        LitSym x -> "'" <> pretty x
        LitNum n -> pretty n
        LitBool b -> if b then "#t" else "#f"
        LitEmpty -> "'()"
        LitPair a b -> "'(" <> go a b <> ")"
            where
                go :: Literal -> Literal -> Doc ann
                go x = \case
                    LitEmpty -> pretty x
                    LitPair y ys -> " " <> pretty x <> go y ys
                    y -> " . " <> pretty y

makeBaseFunctor ''Exp

