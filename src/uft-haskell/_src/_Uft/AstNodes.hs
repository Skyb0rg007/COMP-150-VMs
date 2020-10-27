{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Uft.AstNodes
    ( module Uft.AstNodes
    ) where

import           Data.Bifunctor
import           Data.Char                 (isPrint, ord)
import           Data.Functor.Classes
import           Data.Kind                 (Type)
import           Data.Row.Internal         (Subset, Unconstrained1)
import           Data.Text                 (Text)
-- import           Data.Text.Prettyprint.Doc
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Data.Word                 (Word8)
import           Text.Read                 (Read (readPrec))
import           Type.OpenADT
import           Type.OpenADT.TH
import           Type.VarF
-- import           Uft.Pretty
import           Uft.Primitives
import           Uft.Util

-- * Literals
openADT (drop 3) [d|
    -- | Integer literal: 12 #d123 3.14159 #b010101 #xdeadbeef
    newtype LitNumF a = LitNumF' Double
        deriving Functor
    -- | String literal: "foo" "bar baz"
    newtype LitStrF a = LitStrF' Text
        deriving Functor
    -- | Symbol literal: foo |bar baz|
    newtype LitSymF a = LitSymF' Text
        deriving Functor
    -- | Char literal: #\x #\space #\x64
    newtype LitCharF a = LitCharF' Char
        deriving Functor
    -- | Boolean literal: #t #f #true #false
    newtype LitBoolF a = LitBoolF' Bool
        deriving Functor
    -- | Empty list literal: ()
    data LitEmptyF a = LitEmptyF'
        deriving Functor
    -- | Pair literal: (a . b)
    -- Note that '(1 2 3) produces LitEmpty and LitPair
    data LitPairF a = LitPairF' !a !a
        deriving Functor
    -- | Vector literal: #(a b c)
    newtype LitVectorF a = LitVectorF' (Vector a)
        deriving Functor
    -- | Bytevector literal: #u8(1 2 3)
    newtype LitByteVecF a = LitByteVecF' (Vector Word8)
        deriving Functor
    |]
foldMapM openADTDerive1
    [ ''LitNumF, ''LitStrF, ''LitSymF, ''LitCharF, ''LitBoolF
    , ''LitEmptyF, ''LitPairF, ''LitVectorF, ''LitByteVecF
    ]

-- * Expressions

openADT (drop 3) [d|
    -- | Literals
    -- The literal changes depending on the phase
    newtype ExpLitF lit a = ExpLitF' lit
        deriving Functor
    -- | Variables
    -- The variable representation changes depending on the phase
    newtype ExpVarF name a = ExpVarF' name
        deriving Functor
    -- | Generic set expression
    data ExpSetF name a = ExpSetF' !name !a
        deriving Functor
    -- | Local set expression
    data ExpSetLocalF name a = ExpSetLocalF' !name !a
        deriving Functor
    -- | Global set expression
    data ExpSetGlobalF a = ExpSetGlobalF' !Text !a
        deriving Functor
    -- | If expression
    -- The 'else' branch is optional
    data ExpIfF a = ExpIfF' !a !a !(Maybe a)
        deriving Functor
    -- | While expression
    data ExpWhileF a = ExpWhileF' !a !(Vector a)
        deriving Functor
    -- | Begin expression
    newtype ExpBeginF a = ExpBeginF' (Vector a)
        deriving Functor
    -- | Generic apply expression
    data ExpApplyF a = ExpApplyF' !a !(Vector a)
        deriving Functor
    -- | Application of a primitive
    data ExpApplyPrimF a = ExpApplyPrimF' !(SomePrim a ())
    -- | Application of a name
    data ExpApplyNameF name a = ExpApplyNameF' !name !(Vector a)
        deriving Functor
    -- | Generalized let expression
    -- Paramatrized over the possible let kind
    data ExpLetF lk a = ExpLetF' !lk !(Vector (Text, a)) !(Vector a)
        deriving Functor
    -- | Lambda expression
    data ExpLambdaF a = ExpLambdaF' !(Vector Text) !(Vector a)
        deriving Functor
    |]
instance Functor ExpApplyPrimF where
    fmap f (ExpApplyPrimF' p) = ExpApplyPrimF' (first f p)
instance Show1 ExpApplyPrimF where
    liftShowsPrec sp sl d (ExpApplyPrimF' p) = showParen (d > 10) $
        showString "ExpApplyPrimF' " . liftShowsPrec2 sp sl showsPrec showList 11 p
-- TODO: Eq1 and Ord1 instances for ExpApplyPrimF
foldMapM openADTDerive1
    [ ''ExpLitF, ''ExpVarF, ''ExpSetF, ''ExpSetLocalF
    , ''ExpSetGlobalF, ''ExpIfF, ''ExpWhileF, ''ExpBeginF
    , ''ExpApplyF, ''ExpApplyNameF, ''ExpLetF, ''ExpLambdaF
    ]

openADT (drop 4) [d|
    data StmtValF name exp a = StmtValF' !name !exp
        deriving Functor
    data StmtDefineF name exp a = StmtDefineF' !name !(Vector Text) !exp
        deriving Functor
    newtype StmtExpF exp a = StmtExpF' exp
        deriving Functor
    data StmtCheckExpectF exp a = StmtCheckExpectF' !exp !exp
        deriving Functor
    newtype StmtCheckAssertF exp a = StmtCheckAssertF' exp
        deriving Functor
    |]
foldMapM openADTDerive1
    [ ''StmtValF, ''StmtDefineF, ''StmtExpF
    , ''StmtCheckExpectF, ''StmtCheckAssertF
    ]

-- * Pretty-printing
{- 
instance PrettyF LitNumF where
    prettyF' (LitNumF' n) = styleNum (pretty n)
instance PrettyF LitStrF where
    prettyF' (LitStrF' s) = styleString (dquotes (pretty s))
instance PrettyF LitSymF where
    prettyF' (LitSymF' s) = styleSym (pretty s)
instance PrettyF LitCharF where
    prettyF' (LitCharF' c) 
      | isPrint c = styleChar ("#\\" <> pretty c)
      | otherwise = styleChar ("#\\x" <> pretty (ord c))
instance PrettyF LitBoolF where
    prettyF' (LitBoolF' b) = styleBool (if b then "#t" else "#f")
instance PrettyF LitEmptyF where
    prettyF' LitEmptyF' = "()"
instance PrettyF LitPairF where
    prettyF' (LitPairF' a b) = parens $ a <+> "." <+> b
instance PrettyF LitVectorF where
    prettyF' (LitVectorF' v) = "#(" <> hsep (Vector.toList v) <> ")"
instance PrettyF LitByteVecF where
    prettyF' (LitByteVecF' v) = "#u8(" <> hsep (map pretty (Vector.toList v)) <> ")"

-- | Determine if the literal needs to be printed with a quote-mark
requiresQuoting
    :: forall r x q.
        ( q ~ ("str" .== LitStrF .+ "char" .== LitCharF .+ "num" .== LitNumF)
        , Forall (ApRow r x .\\ ApRow q x) Unconstrained1
        , (ApRow r x .\\ ApRow q x) ~ ApRow (r .\\ q) x
        )
    => VarF r x
    -> Bool
requiresQuoting adt =
    case multiTrialF @q adt of
      Left _  -> False
      Right _ -> True

instance
  ( Forall lit Functor
  , Forall lit PrettyF
  -- x ~ OpenADT lit
  -- q ~ ("str" .== LitStrF .+ "char" .== LitCharF .+ "num" .== LitNumF)
  , Forall (ApRow lit (OpenADT lit) .\\ ApRow ("str" .== LitStrF .+ "char" .== LitCharF .+ "num" .== LitNumF) (OpenADT lit)) Unconstrained1
  , (ApRow lit (OpenADT lit) .\\ ApRow ("str" .== LitStrF .+ "char" .== LitCharF .+ "num" .== LitNumF) (OpenADT lit))
    ~
    (ApRow (lit .\\ ("str" .== LitStrF .+ "char" .== LitCharF .+ "num" .== LitNumF)) (OpenADT lit))
  ) => PrettyF (ExpLitF (OpenADT lit)) where
    prettyF' (ExpLitF' lit) =
        (if requiresQuoting (unfix lit) then squote else "")
        <>
        prettyF lit
instance Pretty name => PrettyF (ExpVarF name) where
    prettyF' (ExpVarF' x) = pretty x
instance Pretty name => PrettyF (ExpSetF name) where
    prettyF' (ExpSetF' x e) = parens $
        styleKw "set" <+> styleVar (pretty x) <+> align e
instance Pretty name => PrettyF (ExpSetLocalF name) where
    prettyF' (ExpSetLocalF' x e) = parens $
        styleKw "set" <+> styleVar (pretty x) <+> align e
instance PrettyF ExpSetGlobalF where
    prettyF' (ExpSetGlobalF' x e) = parens $
        styleKw "set" <+> styleVar (pretty x) <+> align e
instance PrettyF ExpIfF where
    prettyF' (ExpIfF' cond t (Just f)) = parens $
        flatAlt
        (vsep [styleKw "if" <+> cond, indent 2 $ vsep [t, f]])
        (styleKw "if" <+> cond <+> t <+> f)
    prettyF' (ExpIfF' cond t Nothing) = parens $
        flatAlt
        (vsep [styleKw "if" <+> cond, indent 2 t])
        (styleKw "if" <+> cond <+> t)
instance PrettyF ExpWhileF where
    prettyF' (ExpWhileF' cond (Vector.toList -> body)) = parens $
        flatAlt
        (vsep [styleKw "while" <+> cond, indent 2 $ vsep body])
        (styleKw "while" <+> cond <+> hsep body)
instance PrettyF ExpBeginF where
    prettyF' (ExpBeginF' (Vector.toList -> es)) = parens $
        flatAlt
        (vsep [styleKw "begin", indent 2 $ vsep es])
        (hsep $ styleKw "begin" : es)
instance PrettyF ExpApplyF where
    prettyF' (ExpApplyF' f (Vector.toList -> [])) = parens f
    prettyF' (ExpApplyF' f (Vector.toList -> (arg:args))) = parens $
        align $ nest 2 $ sep $ f <+> arg : args
instance Pretty lk => PrettyF (ExpLetF lk) where
    prettyF' (ExpLetF' (pretty -> lk) (Vector.toList -> binds) (Vector.toList -> body)) =
        parens $
            flatAlt
            (vsep [styleKw lk <+> parens (align (vsep (map prettyBinds binds))), indent 2 $ vsep body])
            (styleKw lk <+> parens (hsep (map prettyBinds binds)) <+> hsep body)
        where
            prettyBinds :: (Text, Doc ASTStyle) -> Doc ASTStyle
            prettyBinds (x, e) = brackets $ styleVar (pretty x) <+> align e
instance PrettyF ExpLambdaF where
    prettyF' (ExpLambdaF' (Vector.toList -> args) (Vector.toList -> body)) =
        let args' = parens (hsep (map (styleVar . pretty) args))
         in parens $
             flatAlt
             (vsep [ styleKw "lambda" <+> args'
                   , indent 2 $ align $ vsep body
                   ])
             (styleKw "lambda" <+> args' <+> hsep body)

instance (Pretty name, Forall exp Functor, Forall exp PrettyF)
  => PrettyF (StmtValF name (OpenADT exp)) where
    prettyF' (StmtValF' x e) = parens $
        vsep [ styleKw "val" <+> pretty x, indent 2 $ prettyF e ]
-- instance PrettyF StmtDefineF where
    -- prettyF' (StmtDefineF' f (Vector.toList -> args) body) = parens $
        -- let args' = parens (hsep (map (styleVar . pretty) args))
         -- in vsep [styleKw "define" <+> styleVar (pretty f) <+> args', indent 2 $ prettyF body]
-- instance PrettyF StmtExpF where
    -- prettyF' (StmtExpF' e) = prettyF e
-- instance PrettyF StmtCheckExpectF where
    -- prettyF' (StmtCheckExpectF' e1 e2) = parens $
        -- styleKw "check-expect" <+> prettyF e1 <+> prettyF e2
-- instance PrettyF StmtCheckAssertF where
    -- prettyF' (StmtCheckAssertF' e) = parens $
        -- styleKw "check-assert" <+> prettyF e
 -}
