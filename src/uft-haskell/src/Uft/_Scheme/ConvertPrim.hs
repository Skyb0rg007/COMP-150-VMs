{-# LANGUAGE TemplateHaskell #-}
{-
   Module:      Uft.Scheme.ConvertPrim
   Description: Convert primitives to their own datatype
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.Scheme.ConvertPrim
    ( convertPrim
    , ExpPrimF (ExpPrimF')
    , pattern ExpPrim
    , pattern ExpPrimF
    ) where

import           Control.Monad.Except
import           Data.Deriving
import           Data.Foldable        (foldl')
import           Data.Kind
import           Data.Maybe           (isJust)
import           Data.Text            (Text)
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Primitives
import           Uft.Scheme.Ast
import           Uft.Util

newtype ExpPrimF (a :: Type) = ExpPrimF' Prim
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

derive [deriveOpenADT, deriveEq1, deriveOrd1, deriveShow1, deriveRead1]
    [''ExpPrimF]

instance PrettyF ExpPrimF where
    prettyF' (ExpPrimF' p) = parens $ "prim" <+> pretty (_prim_name p)

convertPrim
    :: forall r m.
        ( '[ExpVarF, ExpLetF, ExpLetRecF] :<: r
        , Applies '[Functor, Foldable, Traversable] r
        , MonadError Text m
        )
   => OpenADT r
   -> m (OpenADT (ExpPrimF : r))
convertPrim = cataM alg where
    alg :: Sum r (OpenADT (ExpPrimF : r)) -> m (OpenADT (ExpPrimF : r))
    alg = \case
        ExpVarF x
          | Just p <- parsePrim x -> pure $ ExpPrim p
        ExpLetF binds body
          | any isJust (fmap (parsePrim . fst) binds) ->
              throwError "Attempt to bind a primitive"
        ExpLetRecF binds body
          | any isJust (fmap (parsePrim . fst) binds) ->
              throwError "Attempt to bind a primitive"
        x -> pure $ Fix (weaken x)

