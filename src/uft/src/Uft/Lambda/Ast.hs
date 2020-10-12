
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Uft.Lambda.Ast
    ( Program
    , Def (..)
    , Exp (..)
    , Alt (..)
    , Literal (..)
    , Pat (..)
    ) where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc

type Program = [Def]

data Def = Def Text [Text] Exp

instance Pretty Def where
    pretty (Def f args body) = align $ vsep
        [ pretty f <+> mconcat (punctuate " " (map pretty args)) <> "="
        , indent 2 $ pretty body
        ]

data Exp
    -- Exp
    = ExpLet [(Text, Exp)] Exp
    | ExpLetRec [(Text, Exp)] Exp
    | ExpLetS [(Text, Exp)] Exp
    | ExpVar Text
    | ExpApp Text [Text]
    | ExpCase Text [Alt]
    | ExpCon Text [Text]
    | ExpLit Literal
    | ExpClosure {- captured -} [Text] {- args -} [Text] {- body -} Exp

instance Pretty Exp where
    pretty = \case
        ExpLet    binds body -> align $ vsep ["let"    <+> prettyBinds binds, " in" <+> pretty body]
        ExpLetRec binds body -> align $ vsep ["letrec" <+> prettyBinds binds, " in" <+> pretty body]
        ExpLetS   binds body -> align $ vsep ["let!"   <+> prettyBinds binds, " in" <+> pretty body]
        ExpVar x -> pretty x
        ExpApp f args -> pretty f <+> hsep (map pretty args)
        ExpCase x alts -> align $ vsep ["case" <+> pretty x <+> "of", indent 2 $ prettyAlts alts]
        ExpCon c args -> pretty c <+> hsep (map pretty args)
        ExpLit lit -> pretty lit
        ExpClosure cap args body ->
            "λ{" <> hsep (map pretty cap) <> "}" <> hsep (map pretty args) <+> "→" <+> pretty body
        where
            prettyBinds = align . vsep . map prettyBind
            prettyBind (x, e) = pretty x <+> "=" <+> pretty e
            prettyAlts = align . vsep . map pretty

data Literal
    = LitInt !Int
    | LitChar !Char

instance Pretty Literal where
    pretty (LitInt n) = pretty n
    pretty (LitChar c) = viaShow c

data Alt = Alt Text Pat Exp

instance Pretty Alt where
    pretty (Alt _ pat e) = "|" <+> pretty pat <+> "→" <+> pretty e

data Pat
    = PatNode Text [Text]
    | PatLit  Literal
    | PatDefault

instance Pretty Pat where
    pretty = \case
        PatNode c args -> pretty c <+> hsep (map pretty args)
        PatLit lit -> pretty lit
        PatDefault -> "_"

