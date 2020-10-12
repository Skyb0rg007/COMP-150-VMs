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

data Exp a
    = ExpLit !Literal
    | ExpVar !a
    | ExpIf !(Exp a) !(Exp a) !(Exp a)
    | ExpLet !a !(Exp a) !(Exp a)
    | ExpSeq !(Exp a) !(Exp a)
    | ExpSet !a !(Exp a)
    | ExpWhile !(Exp a) !(Exp a)
    | ExpFuncode !(Vector a) !(Exp a)
    | ExpFuncall !a !(Vector a)
    | ExpCmd !Cmd !(Vector a)
    | ExpLitCmd !LitCmd !(Vector a) !Literal

data Literal
    = LitNum !Double
    | LitBool !Bool
    | LitSym !Text
    | LitEmpty

-- instance Pretty Exp where
    -- pretty = \case
        -- ExpLit lit -> pretty lit
        -- ExpIf e1 e2 e3 -> "(if" <+> pretty e1 <+> pretty e2 <+> pretty e3 <> ")"
        -- ExpLet kind binds e -> "(" <> pretty kind <+> "(" <> prettyBinds binds <> ")" <+> pretty e <> ")"
        -- ExpVar x -> pretty x
        -- ExpSet x e -> "(" <> pretty x <+> pretty e <> ")"
        -- ExpWhile e1 e2 -> "(while" <+> pretty e1 <+> pretty e2 <> ")"
        -- ExpBegin es -> "(begin" <> foldMap ((" " <>) . pretty) es <> ")"
        -- ExpApply f args -> "(" <> pretty f  <> foldMap ((" " <>) . pretty) args <> ")"
            -- where
                -- prettyBinds :: Foldable f => f (Text, Exp) -> Doc ann
                -- prettyBinds = vsep . foldr (\x acc -> prettyBind x : acc) mempty
                -- prettyBind :: (Text, Exp) -> Doc ann
                -- prettyBind (x, e) = "[" <> pretty x <+> pretty e <> "]"
        -- ExpLambda args e -> "(lambda" <+> "(" <> hsep (toList (fmap pretty args)) <> ")" <+> pretty e <> "))"

-- instance Pretty LetKind where
    -- pretty Let = "let"
    -- pretty LetRec = "letrec"

-- instance Pretty Literal where
    -- pretty = \case
        -- LitSym x -> "'" <> pretty x
        -- LitNum n -> pretty n
        -- LitBool b -> if b then "#t" else "#f"
        -- LitEmpty -> "'()"
        -- LitPair a b -> "'(" <> go a b <> ")"
            -- where
                -- go :: Literal -> Literal -> Doc ann
                -- go x = \case
                    -- LitEmpty -> pretty x
                    -- LitPair y ys -> " " <> pretty x <> go y ys
                    -- y -> " . " <> pretty y

-- makeBaseFunctor ''Exp

