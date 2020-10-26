{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-
   Module:      Uft.Primitives
   Description: Routines for working with SVM primitives
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This module uses the singletons pattern to create depdendently typed primitives
-}

module Uft.Primitives
    ( PrimArgKind (..)
    , PrimRetKind (..)
    , SPrimArgKind (..)
    , SPrimRetKind (..)
    , Primitive (..)
    , SomePrimitive (..)
    , parsePrimitive
    , PrimVec (..)
    , PrimRet (..)
    , SomePrim (..)
    , primCons
    ) where

import           Data.Bifunctor       (Bifunctor (..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Functor.Classes
import           Data.Kind            (Type)
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.TH
import           Data.Text            (Text)

singletons [d|
    -- | DataKind representing the primitive argument kind
    data PrimArgKind = PBinary | PUnary | PNullary | PBinLit | PUnLit
        deriving (Eq, Ord, Show)
    -- | DataKind representing whether the primitive has a return value
    data PrimRetKind = PRet | PNoRet
        deriving (Eq, Ord, Show)
    |]

-- | Datatype representing a SVM primitive with given arg and ret kind
data Primitive (a :: PrimArgKind) (b :: PrimRetKind) where
    Primitive :: (SingI a, SingI b) => Text -> Primitive a b

instance Eq (Primitive a b) where
    Primitive x == Primitive y = x == y
instance Ord (Primitive a b) where
    Primitive x `compare` Primitive y = x `compare` y
instance Show (Primitive a b) where
    showsPrec d (Primitive p) = showParen (d > 10) $
        showString "Primitive @"
        . showsPrec 11 (fromSing $ sing @a)
        . showString " @" . showsPrec 11 (fromSing $ sing @b)
        . showChar ' ' . showsPrec 11 p

-- | Datatype representing some SVM primitive
data SomePrimitive = forall a b. SomePrimitive (Primitive a b)

instance Eq SomePrimitive where
    SomePrimitive (Primitive x) == SomePrimitive (Primitive y) = x == y
instance Ord SomePrimitive where
    SomePrimitive (Primitive x) `compare` SomePrimitive (Primitive y) = x `compare` y
instance Show SomePrimitive where
    showsPrec d (SomePrimitive p) = showParen (d > 10) $
        showString "SomePrimitive " . showsPrec 11 p

-- | Attempt to parse the string into a primitive
parsePrimitive :: Text -> Maybe SomePrimitive
parsePrimitive = flip HashMap.lookup primMap
    where
        primMap :: HashMap Text SomePrimitive
        primMap = HashMap.fromList
            [ ("+",           SomePrimitive (Primitive @PBinary @PRet   "+"))
            , ("-",           SomePrimitive (Primitive @PBinary @PRet   "-"))
            , ("*",           SomePrimitive (Primitive @PBinary @PRet   "*"))
            , ("/",           SomePrimitive (Primitive @PBinary @PRet   "/"))
            , ("//",          SomePrimitive (Primitive @PBinary @PRet   "//"))
            , ("abs",         SomePrimitive (Primitive @PUnary  @PRet   "abs"))
            , ("check",       SomePrimitive (Primitive @PBinLit @PNoRet "check"))
            , ("copyreg",     SomePrimitive (Primitive @PUnary  @PRet   "copyreg"))
            , ("expect",      SomePrimitive (Primitive @PBinLit @PNoRet "expect"))
            , ("getglobal",   SomePrimitive (Primitive @PUnLit  @PRet   "getglobal"))
            , ("hash",        SomePrimitive (Primitive @PUnary  @PRet   "hash"))
            , ("loadliteral", SomePrimitive (Primitive @PUnLit  @PRet   "loadliteral"))
            , ("println",     SomePrimitive (Primitive @PUnary  @PNoRet "println"))
            , ("print",       SomePrimitive (Primitive @PUnary  @PNoRet "print"))
            , ("printu",      SomePrimitive (Primitive @PUnary  @PNoRet "printu"))
            , ("setglobal",   SomePrimitive (Primitive @PBinLit @PNoRet "setglobal"))
            , ("cons",        SomePrimitive (Primitive @PBinary @PRet   "cons"))
            ]

-- | Dependent vector that holds the arguments to a given primitive
data family PrimVec :: PrimArgKind -> a -> Type
data instance PrimVec PNullary a = PVNullary
data instance PrimVec PUnary   a = PVUnary !a
data instance PrimVec PBinary  a = PVBinary !a !a

instance SingI a => Eq1 (PrimVec a) where
    liftEq eq =
        case sing @a of
          SPNullary -> \PVNullary PVNullary -> True
          SPUnary   -> \(PVUnary x) (PVUnary x') -> x `eq` x'
          SPBinary  -> \(PVBinary x y) (PVBinary x' y') -> x `eq` x' && y `eq` y'
          -- SPBinLit  -> \(SPBinLit 
instance SingI a => Ord1 (PrimVec a) where
    liftCompare cmp =
        case sing @a of
          SPNullary -> \PVNullary PVNullary -> EQ
          SPUnary   -> \(PVUnary x) (PVUnary x') -> x `cmp` x'
          SPBinary  -> \(PVBinary x y) (PVBinary x' y') -> x `cmp` x' <> y `cmp` y'
instance SingI a => Show1 (PrimVec a) where
    liftShowsPrec sp _ d =
        case sing @a of
          SPNullary -> \PVNullary -> showString "PVNullary"
          SPUnary   -> \(PVUnary x) -> showParen (d > 10) $
              showString "PVUnary " . sp 11 x
          SPBinary  -> \(PVBinary x y) -> showParen (d > 10) $
              showString "PVBinary " . sp 11 x . showChar ' ' . sp 11 y
instance (SingI a, Eq b) => Eq (PrimVec a b) where
    (==) = eq1
instance (SingI a, Ord b) => Ord (PrimVec a b) where
    compare = compare1
instance (SingI a, Show b) => Show (PrimVec a b) where
    showsPrec = showsPrec1
instance SingI a => Functor (PrimVec a) where
    fmap f =
        case sing @a of
          SPNullary -> \PVNullary      -> PVNullary
          SPUnary   -> \(PVUnary x)    -> PVUnary (f x)
          SPBinary  -> \(PVBinary x y) -> PVBinary (f x) (f y)

-- | Dependent data structure representing the return value of the primitive
data family PrimRet :: PrimRetKind -> a -> Type
data instance PrimRet PRet   a = PRSome !a
data instance PrimRet PNoRet a = PRNone

instance SingI a => Eq1 (PrimRet a) where
    liftEq eq =
        case sing @a of
          SPRet -> \(PRSome x) (PRSome x') -> x `eq` x'
          SPNoRet -> \PRNone PRNone -> True
instance SingI a => Ord1 (PrimRet a) where
    liftCompare cmp =
        case sing @a of
          SPRet -> \(PRSome x) (PRSome x') -> x `cmp` x'
          SPNoRet -> \PRNone PRNone -> EQ
instance SingI a => Show1 (PrimRet a) where
    liftShowsPrec sp _ d =
        case sing @a of
          SPRet -> \(PRSome x) -> showParen (d > 10) $
              showString "PRSome " . sp 11 x
          SPNoRet -> \PRNone -> showString "PRNone"
instance SingI a => Functor (PrimRet a) where
    fmap f =
        case sing @a of
          SPRet   -> \(PRSome x) -> PRSome (f x)
          SPNoRet -> \PRNone     -> PRNone
instance (SingI a, Show b) => Show (PrimRet a b) where
    showsPrec = showsPrec1
instance (SingI a, Eq b) => Eq (PrimRet a b) where
    (==) = eq1
instance (SingI a, Ord b) => Ord (PrimRet a b) where
    compare = compare1

-- | Some primitive application, with the given types as arguments and possible return value
data SomePrim (arg :: Type) (ret :: Type) =
    forall ak rk. SomePrim !(Primitive ak rk) !(PrimVec ak arg) !(PrimRet rk ret)
instance Bifunctor SomePrim where
    bimap f g (SomePrim (Primitive p) pv pr) =
        SomePrim (Primitive p) (fmap f pv) (fmap g pr)
instance Functor (SomePrim arg) where
    fmap = second
instance Eq2 SomePrim where
    liftEq2 eq1 eq2 (SomePrim (p@Primitive{} :: Primitive a b) pv pr) (SomePrim (p'@Primitive{} :: Primitive a' b') pv' pr') =
        case (sing @a %~ sing @a', sing @b %~ sing @b') of
          (Proved Refl, Proved Refl) ->
              p == p' && liftEq eq1 pv pv' && liftEq eq2 pr pr'
          _ -> False
instance Ord2 SomePrim where
    liftCompare2 cmp1 cmp2 (SomePrim (p@Primitive{} :: Primitive a b) pv pr) (SomePrim (p'@Primitive{} :: Primitive a' b') pv' pr') =
        case (sing @a %~ sing @a', sing @b %~ sing @b') of
          (Proved Refl, Proved Refl) ->
              compare p p' <> liftCompare cmp1 pv pv' <> liftCompare cmp2 pr pr'
          _ -> sCompare' (sing @a) (sing @a') <> sCompare' (sing @b) (sing @b')
        where
            sCompare' :: forall a (t1 :: a) (t2 :: a). SOrd a => Sing t1 -> Sing t2 -> Ordering
            sCompare' s1 s2 =
                case sCompare s1 s2 of
                  SLT -> LT
                  SGT -> GT
                  SEQ -> EQ
instance Show2 SomePrim where
    liftShowsPrec2 sp1 sl1 sp2 sl2 d (SomePrim p@Primitive{} pv pr) =
        showParen (d > 10) $
            showString "SomePrim " 
            . showsPrec 11 p . showChar ' '
            . liftShowsPrec sp1 sl1 11 pv . showChar ' '
            . liftShowsPrec sp2 sl2 11 pr
instance Eq a => Eq1 (SomePrim a) where
    liftEq = liftEq2 (==)
instance Ord a => Ord1 (SomePrim a) where
    liftCompare = liftCompare2 compare
instance Show a => Show1 (SomePrim a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
instance (Eq a, Eq b) => Eq (SomePrim a b) where
    (==) = eq1
instance (Ord a, Ord b) => Ord (SomePrim a b) where
    compare = compare1
instance (Show a, Show b) => Show (SomePrim a b) where
    showsPrec = showsPrec1


primCons :: Primitive PBinary PRet
primCons = Primitive "cons"

