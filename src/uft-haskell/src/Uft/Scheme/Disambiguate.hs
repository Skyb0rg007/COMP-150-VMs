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

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Deriving
import           Data.Foldable        (foldl')
import           Data.Functor         ((<&>))
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import           Data.Kind
import           Data.Text            (Text)
import           Type.OpenADT
import           Type.OpenADT.TH
import           Uft.Pretty
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

class Functor f => Disambiguate f where
    disambiguate' :: MonadReader (HashSet Text) m => f (m a) -> m (f a)

instance (Apply Functor fs, Apply Disambiguate fs) => Disambiguate (Sum fs) where
    disambiguate' = apply' @Disambiguate (\r -> fmap r . disambiguate')

instance {-# OVERLAPPABLE #-} (Functor f, Traversable f) => Disambiguate f where
    disambiguate' = sequenceA

instance Disambiguate ExpLetF where
    disambiguate' (ExpLetF' binds body) = do
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
        , ExpVarF :< Delete ExpSetF r
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
        case decompose2 x of
          L1 (ExpSetF' x e) ->
              asks (HashSet.member x) >>= \case
                False -> ExpSetGlobal x <$> e
                True  -> ExpSetLocal x <$> e
          R1 (L1 (ExpVarF' x)) ->
              asks (HashSet.member x) <&> \case
                False -> ExpGetGlobal x
                True  -> ExpGetLocal x
          R1 (R1 x') -> Fix <$> disambiguate' (weaken4 x')
    weaken4 = weaken . weaken . weaken . weaken

