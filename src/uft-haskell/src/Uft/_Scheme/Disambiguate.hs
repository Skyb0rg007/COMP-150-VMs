{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-
   Module:      Uft.Scheme.Disambiguate
   Description: Disambiguate local vs global variables in the AST
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4
-}

module Uft.Scheme.Disambiguate
    ( disambiguate
    , Disambiguate (disambiguate')
    , ExpSetLocalF (ExpSetLocalF')
    , pattern ExpSetLocalF
    , pattern ExpSetLocal
    , ExpGetLocalF (ExpGetLocalF')
    , pattern ExpGetLocalF
    , pattern ExpGetLocal
    , ExpGetGlobalF (ExpGetGlobalF')
    , pattern ExpGetGlobalF
    , pattern ExpGetGlobal
    , ExpSetGlobalF (ExpSetGlobalF')
    , pattern ExpSetGlobalF
    , pattern ExpSetGlobal
    ) where

import           Control.Monad        (when)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Deriving
import           Data.Foldable        (foldl')
import           Data.Functor         ((<&>))
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.Kind
import           Data.Maybe           (isJust)
import           Data.Text            (Text)
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
import           Uft.Primitives
import           Uft.Scheme.Ast
import           Uft.Util

data ExpSetLocalF name a = ExpSetLocalF' !name !a
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

newtype ExpGetLocalF name (a :: Type) = ExpGetLocalF' name
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data ExpSetGlobalF a = ExpSetGlobalF' !Text !a
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

newtype ExpGetGlobalF (a :: Type) = ExpGetGlobalF' Text
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

derive [deriveOpenADT, deriveEq1, deriveOrd1, deriveShow1, deriveRead1]
    [''ExpSetLocalF, ''ExpGetLocalF,''ExpSetGlobalF, ''ExpGetGlobalF]

instance Pretty name => PrettyF (ExpSetLocalF name) where
    prettyF' (ExpSetLocalF' x e) = parens $ "set-local" <+> pretty x <+> e
instance Pretty name => PrettyF (ExpGetLocalF name) where
    prettyF' (ExpGetLocalF' x) = parens $ "get-local" <+> pretty x
instance PrettyF ExpSetGlobalF where
    prettyF' (ExpSetGlobalF' x e) = parens $ "set-global" <+> pretty x <+> e
instance PrettyF ExpGetGlobalF where
    prettyF' (ExpGetGlobalF' x) = parens $ "get-global" <+> pretty x

--
class Functor f => Disambiguate f where
    disambiguate' :: (MonadReader (HashSet Text) m, MonadError Text m)
                  => f (m a)
                  -> m (f a)

instance (Apply Functor fs, Apply Disambiguate fs) => Disambiguate (Sum fs) where
    disambiguate' = apply' @Disambiguate (\r -> fmap r . disambiguate')

instance {-# OVERLAPPABLE #-} (Functor f, Traversable f) => Disambiguate f where
    disambiguate' = sequenceA

instance Disambiguate ExpLetF where
    disambiguate' (ExpLetF' binds body) = do
        when (any isJust (parsePrim . fst <$> binds)) $
            throwError "Attempt to let-bind a primitive"
        let names = hashSetFromFoldable (fst <$> binds)
        binds' <- traverse sequence binds
        ExpLetF' binds' <$> local (HashSet.union names) body

instance Disambiguate ExpLetRecF where
    disambiguate' (ExpLetRecF' binds body) = do
        let names = hashSetFromFoldable (fst <$> binds)
        binds' <- local (HashSet.union names) $ traverse sequence binds
        ExpLetRecF' binds' <$> local (HashSet.union names) body

disambiguate
    :: forall r m new old.
        ( new ~ '[ExpSetGlobalF, ExpGetGlobalF, ExpSetLocalF Text, ExpGetLocalF Text]
        , old ~ '[ExpSetF, ExpVarF]
        , old :<: r
        , Apply Functor r
        , Applies '[Functor, Disambiguate] (new ++ (r \\ old))
        , MonadError Text m
        )
   => OpenADT r
   -> m (OpenADT (new ++ (r \\ old)))
disambiguate e = cata alg e `runReaderT` HashSet.empty where
    alg :: forall m. (MonadError Text m, MonadReader (HashSet Text) m)
        => Sum r (m (OpenADT (new ++ (r \\ old))))
        -> m (OpenADT (new ++ (r \\ old)))
    alg x =
        case decompose2 @ExpSetF @ExpVarF x of
          L1 (ExpSetF x e)
            | isJust (parsePrim x) -> throwError $ "Unable to set primitive " <> x
            | otherwise ->
              asks (HashSet.member x) >>= \case
                False -> ExpSetGlobal x <$> e
                True  -> ExpSetLocal x <$> e
          L1 (ExpVarF x) ->
              asks (HashSet.member x) <&> \case
                False -> ExpGetGlobal x
                True  -> ExpGetLocal x
          R1 x' -> Fix <$> disambiguate' (weaken4 x')

