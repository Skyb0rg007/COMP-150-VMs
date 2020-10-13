
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Uft.UnambiguousScheme.Ast
    ( Stmt (..)
    , Exp (..)
    , ExpF (..)
    , LetKind (..)
    , Literal (..)
    ) where

import           Control.Monad.Except
import           Data.Foldable             (toList)
import           Data.Functor.Foldable.TH  (makeBaseFunctor)
import           Data.Maybe                (isJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Uft.Scheme.Prims          (Prim (..))

type Prog = [Stmt]

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

--

instance {-# OVERLAPPING #-} Pretty Prog where
    pretty = vsep . map pretty

instance Pretty Stmt where
    pretty = \case
        Val x e -> "(val" <+> pretty x <+> pretty e <> ")"
        Define f args e -> "(define" <+> pretty f <+> "(" <> hsep (toList (fmap pretty args)) <> ")" <+> pretty e <>")"
        Exp e -> pretty e
        CheckExpect (_, e1) (_, e2) -> "(check-expect" <+> pretty e1 <+> pretty e2 <> ")"
        CheckAssert (_, e) -> "(check-assert" <+> pretty e <> ")"

instance Pretty Exp where
    pretty = \case
        ExpLit lit -> pretty lit
        ExpLocalVar x -> pretty x
        ExpGlobalVar x -> pretty x
        ExpSetLocal x e -> "(" <> pretty x <+> pretty e <> ")"
        ExpSetGlobal x e -> "(" <> pretty x <+> pretty e <> ")"
        ExpIf e1 e2 e3 -> "(if" <+> pretty e1 <+> pretty e2 <+> pretty e3 <> ")"
        ExpWhile e1 e2 -> "(while" <+> pretty e1 <+> pretty e2 <> ")"
        ExpBegin es -> "(begin" <> foldMap ((" " <>) . pretty) es <> ")"
        ExpFunApply f args -> "(" <> pretty f  <> foldMap ((" " <>) . pretty) args <> ")"
        ExpLet kind binds e -> "(" <> pretty kind <+> "(" <> prettyBinds binds <> ")" <+> pretty e <> ")"
            where
                prettyBinds :: Foldable f => f (Text, Exp) -> Doc ann
                prettyBinds = vsep . foldr (\x acc -> prettyBind x : acc) mempty
                prettyBind :: (Text, Exp) -> Doc ann
                prettyBind (x, e) = "[" <> pretty x <+> pretty e <> "]"
        ExpLambda args e -> "(lambda" <+> "(" <> hsep (toList (fmap pretty args)) <> ")" <+> pretty e <> "))"
        ExpPrimApply f args -> "(" <> pretty f  <> foldMap ((" " <>) . pretty) args <> ")"

instance Pretty LetKind where
    pretty Let = "let"
    pretty LetRec = "letrec"

instance Pretty Literal where
    pretty = \case
        LitSym x -> "'" <> pretty x
        LitNum n -> pretty n
        LitBool b -> if b then "#t" else "#f"
        LitEmpty -> "'()"

makeBaseFunctor ''Exp

