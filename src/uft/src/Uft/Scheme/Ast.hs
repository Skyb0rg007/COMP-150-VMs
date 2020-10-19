
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Uft.Scheme.Ast
    ( Prog
    , Stmt (..)
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

type Prog = [Stmt]

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
    | ExpLet !LetKind !(Vector (Text, Exp)) !(Vector Exp)
    | ExpLambda !(Vector Text) !Exp
    deriving (Show, Eq, Ord)

data LetKind = Let | LetStar | LetRec
    deriving (Show, Eq, Ord)

data Literal
    = LitSym !Text
    | LitNum !Double
    | LitBool !Bool
    | LitPair !Literal !Literal
    | LitEmpty
    deriving (Show, Eq, Ord)

--

instance {-# OVERLAPPING #-} Pretty Prog where
    pretty = vsep . map pretty

instance Pretty Stmt where
    pretty = \case
        Val x e -> "(val" <+> pretty x <+> pretty e <> ")"
        Define f args e -> "(define" <+> pretty f <+> "(" <> hsep (toList (fmap pretty args)) <> ")" <+> pretty e <>")"
        Exp e -> pretty e
        CheckExpect e1 e2 -> "(check-expect" <+> pretty e1 <+> pretty e2 <> ")"
        CheckAssert e -> "(check-assert" <+> pretty e <> ")"

instance Pretty Exp where
    pretty = \case
        ExpLit lit -> pretty lit
        ExpVar x -> pretty x
        ExpSet x e -> "(" <> pretty x <+> pretty e <> ")"
        ExpIf e1 e2 e3 -> "(if" <+> pretty e1 <+> pretty e2 <+> pretty e3 <> ")"
        ExpWhile e1 e2 -> "(while" <+> pretty e1 <+> pretty e2 <> ")"
        ExpBegin es -> "(begin" <> foldMap ((" " <>) . pretty) es <> ")"
        ExpApply f args -> "(" <> pretty f  <> foldMap ((" " <>) . pretty) args <> ")"
        ExpLet kind binds e -> "(" <> pretty kind <+> "(" <> prettyBinds binds <> ")" <+> hsep (toList (fmap pretty e)) <> ")"
            where
                prettyBinds :: Foldable f => f (Text, Exp) -> Doc ann
                prettyBinds = vsep . foldr (\x acc -> prettyBind x : acc) mempty
                prettyBind :: (Text, Exp) -> Doc ann
                prettyBind (x, e) = "[" <> pretty x <+> pretty e <> "]"
        ExpLambda args e -> "(lambda" <+> "(" <> hsep (toList (fmap pretty args)) <> ")" <+> pretty e <> "))"

instance Pretty LetKind where
    pretty Let = "let"
    pretty LetStar = "let*"
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

