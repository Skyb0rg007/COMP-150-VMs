
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Uft.Primitives
    ( 
    ) where

import           Data.Bimap         (Bimap)
import qualified Data.Bimap         as Bimap
import           Data.Kind          (Type)
import           Data.Singletons
import           Data.Singletons.TH (singletons)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Type.OpenADT.TH

singletons [d|
    data PrimArgKind = PBinary | PUnary | PNullary | PBinLit | PBUnLit
    data PrimRetKind = PRet | PNoRet
    |]

data Primitive (a :: PrimArgKind) (b :: PrimRetKind) =
    Primitive (Sing a) (Sing b) Text

data SomePrimitive = forall a b. SomePrimitive (Primitive a b)

instance Eq SomePrimitive where
    SomePrimitive (Primitive _ _ x) == SomePrimitive (Primitive _ _ y) = x == y
instance Ord SomePrimitive where
    SomePrimitive (Primitive _ _ x) `compare` SomePrimitive (Primitive _ _ y) = x `compare` y

primitives_ :: [(Text, PrimArgKind, PrimRetKind)]
primitives_ =
    [ (,,) "+" PBinary PRet
    , (,,) "-" PBinary PRet
    ]

primBimap :: Bimap Text SomePrimitive
primBimap = Bimap.fromList $ map go primitives_
    where
        go (x, (FromSing ak), (FromSing rk)) = (x, SomePrimitive (Primitive ak rk x))

data family PrimVec :: PrimArgKind -> a -> Type
data instance PrimVec PNullary a = PVNullary
data instance PrimVec PUnary   a = PVUnary !a
data instance PrimVec PBinary  a = PVBinary !a !a

instance SingI a => Functor (PrimVec a) where
    fmap f = case sing @a of
               SPNullary -> \PVNullary      -> PVNullary
               SPUnary   -> \(PVUnary x)    -> PVUnary (f x)
               SPBinary  -> \(PVBinary x y) -> PVBinary (f x) (f y)

data family PrimRet :: PrimRetKind -> a -> Type
data instance PrimRet PRet   a = PRSome !a
data instance PrimRet PNoRet a = PRNone

instance SingI a => Functor (PrimRet a) where
    fmap f = case sing @a of
               SPRet   -> \(PRSome x) -> PRSome (f x)
               SPNoRet -> \PRNone     -> PRNone

data SomePrimExp (a :: Type) =
    forall ak rk. SomePrimExp !(Primitive ak rk) !(PrimVec ak a) !(PrimRet rk a)
instance Functor SomePrimExp where
    fmap f (SomePrimExp (Primitive ak rk p) pv pr) =
        withSingI ak $ withSingI rk $
            SomePrimExp (Primitive ak rk p) (fmap f pv) (fmap f pr)

openADT (drop 3) [d|
    newtype ExpPrimF a = ExpPrimF' (SomePrimExp a)
    |]


