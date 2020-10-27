
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Type.VarF
    (
    -- * VarF
      VarF (..)
    , varFAlg
    , OpenAlg
    -- * VarF'
    , VarF' (..)
    , varFAlg'
    -- * FlipApp
    , FlipApp (FlipApp)
    -- * ApRow
    , ApRow
    , apIndexLaw
    , apRemoveLaw
    , apExtendLaw
    , apExtendIndexLaw
    , apUniqueLaw
    -- * Lifting of functions in Data.Row
    , caseonF
    , coerceVarF
    , diversifyF
    , eraseF
    , eraseWithLabelsF
    , eraseZipF
    , impossibleF
    , labelsF
    , multiTrialF
    , traverseF
    , trialF
    , viewF
    -- * Re-export
    , Label (..)
    ) where

import           Data.Bifunctor         (bimap, first)
import           Data.Constraint        ((:-), withDict, (\\))
import           Data.Constraint.Unsafe (unsafeCoerceConstraint)
import           Data.Functor.Classes
import           Data.Functor.Const     (Const (Const, getConst))
import           Data.Functor.Product   (Product (Pair))
import           Data.Kind              (Constraint, Type)
import           Data.Proxy             (Proxy (Proxy))
import           Data.Coerce (Coercible)
import           Data.Row
import           Data.Row.Internal
import           Data.Row.Switch
import           Data.Row.Variants
import           Data.String            (IsString)
import           GHC.TypeLits           (Symbol)
import           Text.Read

type family ApRow (r :: Row (k -> Type)) (x :: k) :: Row Type where
    ApRow (R r) x = R (ApLT r x)

type family ApLT (r :: [LT (k -> Type)]) (x :: k) :: [LT Type] where
    ApLT '[] _ = '[]
    ApLT (lbl :-> f : rest) x = lbl :-> f x : ApLT rest x

-- | A law about 'ApRow'
-- Type-level equivalent of @fmap ($ x) fs !! idx === (fs !! idx) x@
apIndexLaw
    :: forall k (l :: Symbol) (ρ :: Row (k -> Type)) (x :: k).
       KnownSymbol l :- ((ApRow ρ x .! l) ~ (ρ .! l) x)
apIndexLaw = unsafeCoerceConstraint

-- | A law about 'ApRow'
-- Type-level equivalent of @deleteAt idx (fmap ($ x) fs) === fmap ($ x) (deleteAt fs idx)@
apRemoveLaw
    :: forall k (l :: Symbol) (ρ :: Row (k -> Type)) (x :: k).
       KnownSymbol l :- ((ApRow ρ x .- l) ~ ApRow (ρ .- l) x)
apRemoveLaw = unsafeCoerceConstraint

-- | A law about 'ApRow'
-- Type-level equivalent of @(l, t) : fmap ($ x) fs === fmap ($ x) ()@
apExtendLaw
    :: forall k (l :: Symbol) (ρ :: Row (k -> Type)) (x :: k) (τ :: k -> Type).
       KnownSymbol l :- (Extend l (τ x) (ApRow ρ x) ~ ApRow (Extend l τ ρ) x)
apExtendLaw = unsafeCoerceConstraint

-- | A law about 'ApRow'
apExtendIndexLaw
    :: forall k (l :: Symbol) (ρ :: Row (k -> Type)) (x :: k) (τ :: k -> Type).
       KnownSymbol l :- ((ApRow (Extend l τ ρ) x .! l) ~ τ x)
apExtendIndexLaw = unsafeCoerceConstraint

-- | A law about 'ApRow'
apUniqueLaw
    :: forall k (ρ :: Row (k -> Type)) (x :: k).
       AllUniqueLabels ρ :- AllUniqueLabels (ApRow ρ x)
apUniqueLaw = unsafeCoerceConstraint

newtype VarF (r :: Row (k -> Type)) (x :: k) = VarF { unVarF :: Var (ApRow r x) }

deriving instance Forall (ApRow r x) Eq => Eq (VarF r x)
deriving instance (Forall (ApRow r x) Eq, Forall (ApRow r x) Ord) => Ord (VarF r x)
deriving instance Forall (ApRow r x) Show => Show (VarF r x)

-- Helpers for defining Functor instance
newtype FlipApp a f = FlipApp (f a)

newtype VarF' (x :: k) (r :: Row (k -> Type)) = VarF' { unVarF' :: Var (ApRow r x) }

varFAlg
    :: forall (c :: (Type -> Type) -> Constraint) (r :: Row (Type -> Type)) (x :: Type) (y :: Type).
       Forall r c
    => (forall f. c f => f x -> y)
    -> VarF r x
    -> y
varFAlg f = getConst . go . VarF' . unVarF
    where
        go :: VarF' x r -> Const y r
        go = metamorph @(Type -> Type) @r @c @Either @(VarF' x) @(Const y) @(FlipApp x) proxy doNil doUncons doCons
        proxy :: Proxy (Proxy (FlipApp x), Proxy Either)
        proxy = Proxy
        doNil :: VarF' x Empty -> Const y Empty
        doNil = impossible . unVarF'
        doUncons :: forall l τ ρ. (KnownSymbol l, c τ, HasType l τ ρ)
                 => Label l
                 -> VarF' x ρ
                 -> Either (VarF' x (ρ .- l)) (FlipApp x τ)
        doUncons l (VarF' v) =
            case trial v l of
              Right tx -> Right (FlipApp tx) \\ apIndexLaw @Type @l @ρ @x
              Left v' -> Left (VarF' v') \\ apRemoveLaw @Type @l @ρ @x
        doCons :: forall l τ ρ. (KnownSymbol l, c τ, FrontExtends l τ ρ, AllUniqueLabels (Extend l τ ρ))
               => Label l
               -> Either (Const y ρ) (FlipApp x τ)
               -> Const y (Extend l τ ρ)
        doCons _ (Right (FlipApp tx)) = Const (f tx)
        doCons _ (Left  (Const y))    = Const y

varFAlg'
    :: forall (r :: Row (Type -> Type)) (x :: Type) (y :: Type).
       Forall r Unconstrained1
    => (forall f . Unconstrained1 f => f x -> y)
    -> VarF r x
    -> y
varFAlg' = varFAlg @Unconstrained1 @r @x @y

instance Forall r Functor => Functor (VarF r) where
    fmap :: forall a b. (a -> b) -> VarF r a -> VarF r b
    fmap f = VarF . unVarF' . go . VarF' . unVarF
        where
            go :: VarF' a r -> VarF' b r
            go = metamorph @(Type -> Type) @r @Functor @Either @(VarF' a) @(VarF' b)
                 proxy doNil doUncons doCons
            proxy :: Proxy (Proxy (FlipApp a), Proxy Either)
            proxy = Proxy
            doNil :: VarF' a Empty -> VarF' b Empty
            doNil = impossible . unVarF'
            doUncons :: forall l τ ρ. (KnownSymbol l, Functor τ, HasType l τ ρ)
                     => Label l
                     -> VarF' a ρ
                     -> Either (VarF' a (ρ .- l)) (FlipApp a τ)
            doUncons l (VarF' v) =
                case trial v l of
                  Left v' -> Left (VarF' v') \\ apRemoveLaw @Type @l @ρ @a
                  Right x -> Right (FlipApp x) \\ apIndexLaw @Type @l @ρ @a
            doCons :: forall l τ ρ. (KnownSymbol l, Functor τ, FrontExtends l τ ρ, AllUniqueLabels (Extend l τ ρ))
                   => Label l
                   -> Either (VarF' b ρ) (FlipApp a τ)
                   -> VarF' b (Extend l τ ρ)
            doCons l (Left (VarF' v)) = VarF' (extend @(τ b) l v) \\
                apExtendLaw @Type @l @ρ @b @τ
            doCons l (Right (FlipApp x)) = VarF' (IsJust l (fmap f x)) \\
                apExtendIndexLaw @Type @l @ρ @b @τ \\
                apUniqueLaw @Type @(Extend l τ ρ) @b

instance Forall r Eq1 => Eq1 (VarF r) where
    liftEq :: forall a b. (a -> b -> Bool) -> VarF r a -> VarF r b -> Bool
    liftEq f (VarF x) (VarF y) = getConst $ go $ Pair (VarF' x) (VarF' y)
        where
            go = metamorph @_ @r @Eq1 @Either @(Product (VarF' a) (VarF' b)) @(Const Bool)
                 proxy doNil doUncons doCons
            proxy :: Proxy (Proxy (Const Bool), Proxy Either)
            proxy = Proxy
            doNil :: Product (VarF' a) (VarF' b) Empty -> Const Bool Empty
            doNil (Pair (VarF' x) _) = impossible x
            doUncons :: forall l τ ρ. (KnownSymbol l, Eq1 τ, HasType l τ ρ)
                     => Label l
                     -> Product (VarF' a) (VarF' b) ρ
                     -> Either (Product (VarF' a) (VarF' b) (ρ .- l)) (Const Bool τ)
            doUncons l (Pair (VarF' x) (VarF' y)) =
                withDict (apIndexLaw @Type @l @ρ @a) $
                withDict (apIndexLaw @Type @l @ρ @b) $
                withDict (apRemoveLaw @Type @l @ρ @a) $
                withDict (apRemoveLaw @Type @l @ρ @b) $
                case (trial x l, trial y l) of
                  (Right x', Right y') -> Right $ Const $ liftEq f x' y'
                  (Left x',  Left y')  -> Left $ Pair (VarF' x') (VarF' y')
                  _                    -> Right $ Const False
            doCons :: forall l τ ρ. (KnownSymbol l, Eq1 τ, FrontExtends l τ ρ, AllUniqueLabels (Extend l τ ρ))
                   => Label l
                   -> Either (Const Bool ρ) (Const Bool τ)
                   -> Const Bool (Extend l τ ρ)
            doCons _ (Left  (Const b)) = Const b
            doCons _ (Right (Const b)) = Const b

instance (Forall r Eq1, Forall r Ord1) => Ord1 (VarF r) where
    liftCompare :: forall a b. (a -> b -> Ordering) -> VarF r a -> VarF r b -> Ordering
    liftCompare f (VarF x) (VarF y) = getConst $ go $ Pair (VarF' x) (VarF' y)
        where
            go = metamorph @_ @r @Ord1 @Either @(Product (VarF' a) (VarF' b)) @(Const Ordering)
                 proxy doNil doUncons doCons
            proxy :: Proxy (Proxy (Const Ordering), Proxy Either)
            proxy = Proxy
            doNil :: Product (VarF' a) (VarF' b) Empty -> Const Ordering Empty
            doNil (Pair (VarF' x) _) = impossible x
            doUncons :: forall l τ ρ. (KnownSymbol l, Ord1 τ, HasType l τ ρ)
                     => Label l
                     -> Product (VarF' a) (VarF' b) ρ
                     -> Either (Product (VarF' a) (VarF' b) (ρ .- l)) (Const Ordering τ)
            doUncons l (Pair (VarF' x) (VarF' y)) =
                withDict (apIndexLaw @Type @l @ρ @a) $
                withDict (apIndexLaw @Type @l @ρ @b) $
                withDict (apRemoveLaw @Type @l @ρ @a) $
                withDict (apRemoveLaw @Type @l @ρ @b) $
                case (trial x l, trial y l) of
                  (Right x', Right y') -> Right $ Const $ liftCompare f x' y'
                  (Right x', Left y')  -> Right $ Const LT
                  (Left x', Right y')  -> Right $ Const GT
                  (Left x', Left y')   -> Left $ Pair (VarF' x') (VarF' y')
            doCons :: forall l τ ρ. (KnownSymbol l, Ord1 τ, FrontExtends l τ ρ, AllUniqueLabels (Extend l τ ρ))
                   => Label l
                   -> Either (Const Ordering ρ) (Const Ordering τ)
                   -> Const Ordering (Extend l τ ρ)
            doCons _ (Left  (Const o)) = Const o
            doCons _ (Right (Const o)) = Const o

instance Forall r Show1 => Show1 (VarF r) where
    liftShowsPrec
        :: forall a.
           (Int -> a -> ShowS)
        -> ([a] -> ShowS)
        -> Int
        -> VarF r a
        -> ShowS
    liftShowsPrec sa sl p = varFAlg @Show1 @r @a @ShowS f
        where
            f :: forall τ. Show1 τ => τ a -> ShowS
            f x = showParen (p > 10) $ showString "VarF " . liftShowsPrec sa sl p x

type OpenAlg r l f v =
    ( (ApRow r v .! l) ~ f v 
    , AllUniqueLabels (ApRow r v)
    )

impossibleF :: VarF Empty a -> b
impossibleF = impossible . unVarF

diversifyF
    :: forall r' x r.
       ((ApRow r x .\/ ApRow r' x) ~ ApRow (r .\/ r') x)
    => VarF r x
    -> VarF (r .\/ r') x
diversifyF = VarF . diversify @(ApRow r' x) @(ApRow r x) . unVarF

eraseF
    :: forall c r x b.
       Forall (ApRow r x) c
    => (forall a. c a => a -> b)
    -> VarF r x
    -> b
eraseF f = snd @String . eraseWithLabelsF @c f

eraseWithLabelsF
    :: forall c r x s b.
       (Forall (ApRow r x) c, IsString s)
    => (forall a. c a => a -> b)
    -> VarF r x
    -> (s, b)
eraseWithLabelsF f = eraseWithLabels @c f . unVarF

eraseZipF
    :: forall c r x b.
       Forall (ApRow r x) c
    => (forall a. c a => a -> a -> b)
    -> VarF r x
    -> VarF r x
    -> Maybe b
eraseZipF f (VarF a) (VarF b) = eraseZip @c f a b

traverseF
    :: forall c f r x.
       (Forall (ApRow r x) c, Applicative f)
    => (forall a. c a => a -> f a)
    -> VarF r x
    -> f (VarF r x)
traverseF f = fmap VarF . Data.Row.Variants.traverse @c f . unVarF

labelsF :: forall r c s. (IsString s, Forall r c) => [s]
labelsF = labels @r @c @s

coerceVarF
    :: forall r1 r2 x y.
       BiForall (ApRow r1 x) (ApRow r2 y) Coercible
    => VarF r1 x
    -> VarF r2 y
coerceVarF = VarF . coerceVar . unVarF

trialF
    :: forall k l (r :: Row (k -> Type)) x. KnownSymbol l
    => VarF r x
    -> Label l
    -> Either (VarF (r .- l) x) (ApRow r x .! l)
trialF (VarF v) l =
    withDict (apRemoveLaw @k @l @r @x) $
        first VarF $ trial v l

trialF'
    :: forall k l (r :: Row (k -> Type)) x. KnownSymbol l
    => VarF r x
    -> Label l
    -> Maybe (ApRow r x .! l)
trialF' (VarF v) l = trial' v l

caseonF
    :: BiForall r (ApRow v x) (AppliesTo y)
    => Rec r
    -> VarF v x
    -> y
caseonF r = caseon r . unVarF

multiTrialF
    :: forall u v x.
       ( AllUniqueLabels (ApRow u x)
       , Forall (ApRow v x .\\ ApRow u x) Unconstrained1
       , (ApRow v x .\\ ApRow u x) ~ ApRow (v .\\ u) x
       )
    => VarF v x
    -> Either (VarF (v .\\ u) x) (VarF u x)
multiTrialF = bimap VarF VarF . multiTrial . unVarF

viewF
    :: KnownSymbol l
    => Label l
    -> VarF r x
    -> Maybe (ApRow r x .! l)
viewF l (VarF v) = view l v

