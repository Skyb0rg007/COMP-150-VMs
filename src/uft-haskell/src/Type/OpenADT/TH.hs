{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-
   Module:      Type.OpenADT.TH
   Description: Extensible recursive sum type - Template Haskell helpers
   Copyright:   Skye Soss 2020
   License:     MIT
   Maintainer:  skyler.soss@gmail.com
   Stability:   experimental
   Portability: ghc-8.8.4

   This implementation is modified from the `open-adt` package: https://github.com/woehr/open-adt
-}

module Type.OpenADT.TH
    ( deriveOpenADT
    ) where

import           Control.Monad
import           Data.Foldable          (foldl')
import           Language.Haskell.TH
import           Type.OpenADT
import           Uft.Util               (unsnoc)

deriveOpenADT :: Name -> Q [Dec]
deriveOpenADT name = do
    TyConI dec <- reify name
    goDec dec
    where
        bndrName :: TyVarBndr -> Name
        bndrName (PlainTV n) = n
        bndrName (KindedTV n _) = n
        bndrToVar :: TyVarBndr -> Type
        bndrToVar = VarT . bndrName
        funApp :: Q Type -> Q Type -> Q Type
        funApp a b = appT (appT arrowT a) b
        -- | Matches normal and record constructors
        nrC :: Con -> Maybe (Name, [Type])
        nrC (NormalC conName bangTypes) = Just (conName, map snd bangTypes)
        nrC (RecC conName varBangTypes) = Just (conName, map (\(_, _, t) -> t) varBangTypes)
        nrC _ = Nothing
        -- | Ensure the last type variable has kind 'Type'
        kindStarLast :: [TyVarBndr] -> [TyVarBndr]
        kindStarLast ts
          | Just (initTys, lastTy) <- unsnoc ts =
              let lastTy' = case lastTy of { PlainTV x -> KindedTV x StarT; _ -> lastTy }
               in initTys ++ [lastTy']
          | otherwise = error "kindStarLast: Empty list of binders"
        -- | Get the relevant info from the datatype
        getInfo :: Dec -> (Name, [TyVarBndr], Name, [Type])
        getInfo = \case
            DataD [] nm (kindStarLast -> tyVarBndrs) Nothing [nrC -> Just (conName, conTypes)] _ ->
                (nm, tyVarBndrs, conName, conTypes)
            NewtypeD [] nm (kindStarLast -> tyVarBndrs) Nothing (nrC -> Just (conName, conTypes)) _ ->
                (nm, tyVarBndrs, conName, conTypes)
            _ -> error "Expected a data or newtype declaration with one constructor"
        -- | Main routine
        goDec :: Dec -> Q [Dec]
        goDec dec = do
            let (tyName, tyVarBndrs, conName, conTypes) = getInfo dec
            when (last (nameBase tyName) /= 'F') $
                fail $ "Expected type name to end with 'F' - got \"" ++ nameBase tyName ++ "\""
            when (drop (length (nameBase conName) - 2) (nameBase conName) /= "F'") $
                fail $ "Expected type constructor name to end with F' - got \"" ++ nameBase conName ++ "\""
            let patName :: Name
                patName = mkName $ init $ nameBase tyName
            let patFName :: Name
                patFName = mkName $ nameBase tyName

            argNames :: [Name] <- replicateM (length conTypes) (newName "a")
            rName :: Name <- newName "r"
            let lastVar :: Q Type
                lastVar = pure $ bndrToVar $ last tyVarBndrs
                -- | The type constructor applied to all but the last type
                appliedTyCon :: Q Type
                appliedTyCon = pure $ foldl' AppT (ConT tyName) (init (map bndrToVar tyVarBndrs))
                -- | The value constructor applied to all the variable args
                appliedConExp :: Q Exp
                appliedConExp = pure $ foldl' AppE (ConE conName) (fmap VarE argNames)
                -- | The value constructor applied to all the variable args
                appliedConPat :: Q Pat
                appliedConPat = pure $ ConP conName (fmap VarP argNames)
                -- | The patF constructor applied to all the variable args
                appliedConPatF :: Q Pat
                appliedConPatF = pure $ ConP patFName (fmap VarP argNames)

            let patFType :: Q Type
                patFType = forallT bndrs ctx ty
                    where
                        bndrs :: [TyVarBndr]
                        bndrs = PlainTV rName : tyVarBndrs
                        ctx :: Q [Type]
                        ctx = fmap pure [t| $appliedTyCon :< $(varT rName) |]
                        ty :: Q Type
                        ty = foldr funApp retTy (map pure conTypes)
                        retTy :: Q Type
                        retTy = [t| Sum $(varT rName) $lastVar |]
                patFBody :: Q Pat
                patFBody = [p| (Sum $appliedConPat) |]
                patFClause :: Q Exp
                patFClause = [| Sum $appliedConExp |]

            let patType :: Q Type
                patType = forallT bndrs ctx ty
                    where
                        bndrs :: [TyVarBndr]
                        bndrs = PlainTV rName : tyVarBndrs
                        ctx :: Q [Type]
                        ctx = fmap pure [t| ($appliedTyCon :< $(varT rName), $lastVar ~ Fix (Sum $(varT rName))) |]
                        ty :: Q Type
                        ty = foldr funApp lastVar (map pure conTypes)
                patBody :: Q Pat
                patBody = [p| Fix $appliedConPatF |]

            sequence [ patSynSigD patFName patFType
                     , patSynD patFName
                           (prefixPatSyn argNames)
                           (explBidir [clause (fmap varP argNames) (normalB patFClause) []])
                           patFBody
                     , patSynSigD patName patType
                     , patSynD patName
                           (prefixPatSyn argNames)
                           implBidir
                           patBody
                     ]

