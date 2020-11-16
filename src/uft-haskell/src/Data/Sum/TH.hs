{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK hide #-}
{-
   Module:      Type.OpenADT.TH
   Description: Extensible recursive sum type - Template Haskell helpers
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This implementation is taken from the `fastsum` package: https://github.com/patrickt/fastsum
-}
module Data.Sum.TH
    ( mkElemIndexTypeFamily
    , mkApplyInstance
    ) where

import           Data.Foldable (asum)
import           Data.Kind           (Type)
import           GHC.TypeLits        (ErrorMessage (..), Nat, TypeError)
import           Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH (Type)
import           Unsafe.Coerce       (unsafeCoerce)

{- This generates a type family of the form

type family ElemIndex (t :: GHC.Types.Type
                            -> GHC.Types.Type) (ts :: [GHC.Types.Type
                                                       -> GHC.Types.Type]) :: Nat where
  ElemIndex t0 ('(:) t0 _) = 0
  ElemIndex t1 ('(:) t0 ('(:) t1 _)) = 1
  ElemIndex t2 ('(:) t0 ('(:) t1 ('(:) t2 _))) = 2
  ElemIndex t3 ('(:) t0 ('(:) t1 ('(:) t2 ('(:) t3 _)))) = 3
  ElemIndex t4 ('(:) t0 ('(:) t1 ('(:) t2 ('(:) t3 ('(:) t4 _))))) = 4
  etc...
  ElemIndex t ts = TypeError ('(:$$:) ('(:<>:) ('(:<>:) ('Text "'") ('ShowType t)) ('Text "' is not a member of the type-level list")) ('ShowType ts))

-}
mkElemIndexTypeFamily :: Integer -> DecsQ
mkElemIndexTypeFamily paramN = do
  -- Start by declaring some names.
  let elemIndex = mkName "ElemIndex"
      t = mkName "t"
      ts = mkName "ts"
      -- Helper for building more readable type names rather than verbose gensyms
      mkT = pure . VarT . mkName . ('t' :) . show
      -- We want to make the kind signatures explicit here.
      binders = [kindedTV t  <$> [t| Type -> Type |] , kindedTV ts <$> [t| [Type -> Type] |] ]
      -- This family ends up returning a Nat.
      resultKind = kindSig <$> [t| Nat |]
      -- We have to build n ElemIndex entries.
      equations = fmap buildEquation [0..pred paramN] ++ [errorCase]
      errorBody = [t|
        TypeError ('Text "'" ':<>: ('ShowType $(varT t)) ':<>:
                   'Text "' is not a member of the type-level list" ':$$:
                   'ShowType $(varT ts))
        |]
      -- The tySynEqn API changed in 2.15 so we need a guard here.
      -- buildEquation a single family instance equation; it uses lhsMatch
      -- to do so, making a type of the form 'ElemIndex n (n ': n0 : _)
      -- errorCase is invoked above to provide a readable error
      buildEquation n = tySynEqn Nothing (lhsMatch n) . litT . numTyLit $ n
      lhsMatch n = [t| $(conT elemIndex) $(mkT n) $(typeListT WildCardT <$> traverse mkT [0..n]) |]
      errorCase = tySynEqn Nothing [t| $(conT elemIndex) $(varT t) $(varT ts) |] errorBody
      -- buildEquation n = tySynEqn (lhsMatch n) (litT . numTyLit $ n)
      -- lhsMatch n = [mkT n, typeListT WildCardT <$> traverse mkT [0..n] ]
      -- errorCase = tySynEqn [varT t, varT ts] errorBody

  fmap pure =<< closedTypeFamilyD elemIndex
    <$> sequenceA binders
    <*> resultKind
    <*> pure Nothing
    <*> pure equations

{-

instance (constraint f0, constraint f1) => Apply constraint '[f0, f1] where
    apply f (Sum 0 r) = f (unsafeCoerce r :: f0)
    apply f (Sum 1 r) = f (unsafeCoerce r :: f1)

    coapply f r = fmap ((.) (Sum' 0) unsafeCoerce) (f @f0)
              <|> fmap ((.) (Sum' 1) unsafeCoerce) (f @f1)

-}
mkApplyInstance :: Integer -> Dec
mkApplyInstance paramN =
  InstanceD Nothing (AppT constraint <$> typeParams) (AppT (AppT (ConT applyC) constraint) (typeListT PromotedNilT typeParams))
    [ FunD apply (zipWith mkClause [0..] typeParams)
    , PragmaD (InlineP apply Inlinable FunLike AllPhases)
    , FunD coapply [Clause [VarP f] (NormalB coapplyExp) []]
    , PragmaD (InlineP coapply Inlinable FunLike AllPhases)
    ]
  where typeParams = VarT . mkName . ('f' :) . show <$> [0..pred paramN]
        applyC = mkName "Apply"
        apply = mkName "apply"
        coapply = mkName "coapply"
        f = mkName "f"
        r = mkName "r"
        union = mkName "Sum'"
        constraint = VarT (mkName "constraint")
        a = VarT (mkName "a")
        mkClause i nthType = Clause
          [ VarP f, ConP union [ LitP (IntegerL i), VarP r ] ]
          (NormalB (AppE (VarE f) (SigE (AppE (VarE 'unsafeCoerce) (VarE r)) (AppT nthType a))))
          []
        coapplyExp = AppE (VarE 'asum) (ListE (zipWith appSum' [0..] typeParams))
        appSum' i nthType =
            AppE (AppE (VarE 'fmap)
              (AppE (AppE (VarE '(.))
                (AppE (ConE union) (LitE (IntegerL i))))
                (VarE 'unsafeCoerce)))
              (AppTypeE (VarE f)
                nthType)

typeListT :: TH.Type -> [TH.Type] -> TH.Type
typeListT = foldr (AppT . AppT PromotedConsT)
