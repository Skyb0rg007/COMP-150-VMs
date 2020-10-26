
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Type.OpenADT.TH
    ( deriveOpenADT
    ) where

import           Control.Monad
import           Data.Char           (toLower)
import           Data.Foldable       (foldl')
import           Language.Haskell.TH
import           Type.OpenADT
import           Uft.Util            (unsnoc)

deriveOpenADT :: (String -> String) -> Name -> Q [Dec]
deriveOpenADT convert name = do
    TyConI dec <- reify name
    goDec dec
    where
        getInfo :: Dec -> (Name, [TyVarBndr], Name, [Type])
        getInfo = \case
            DataD [] nm tyVarBndrs Nothing [NormalC (unFresh -> conName) bangTypes] _
              | Just (tyVarBndrsInit, tyVarBndrsTail) <- unsnoc tyVarBndrs ->
                  let tyVarBndrsLast' = case tyVarBndrsTail of
                                          PlainTV x -> KindedTV x StarT
                                          _ -> tyVarBndrsTail
                      tyVarBndrs' = tyVarBndrsInit ++ [tyVarBndrsLast']
                   in ( nm
                      , tyVarBndrs'
                      , conName
                      , map (\(_, t) -> t) bangTypes
                      )
            DataD [] nm tyVarBndrs Nothing [RecC (unFresh -> conName) varBangTypes] _
              | Just (tyVarBndrsInit, tyVarBndrsTail) <- unsnoc tyVarBndrs ->
                  let tyVarBndrsLast' = case tyVarBndrsTail of
                                          PlainTV x -> KindedTV x StarT
                                          _ -> tyVarBndrsTail
                      tyVarBndrs' = tyVarBndrsInit ++ [tyVarBndrsLast']
                   in ( nm
                      , tyVarBndrs'
                      , conName
                      , map (\(_, _, t) -> t) varBangTypes
                      )
            NewtypeD [] nm tyVarBndrs Nothing (NormalC (unFresh -> conName) bangTypes) _
              | Just (tyVarBndrsInit, tyVarBndrsTail) <- unsnoc tyVarBndrs ->
                  let tyVarBndrsLast' = case tyVarBndrsTail of
                                          PlainTV x -> KindedTV x StarT
                                          _ -> tyVarBndrsTail
                      tyVarBndrs' = tyVarBndrsInit ++ [tyVarBndrsLast']
                   in ( nm
                      , tyVarBndrs'
                      , conName
                      , map (\(_, t) -> t) bangTypes
                      )
            NewtypeD [] nm tyVarBndrs Nothing (RecC (unFresh -> conName) varBangTypes) _
              | Just (tyVarBndrsInit, tyVarBndrsTail) <- unsnoc tyVarBndrs ->
                  let tyVarBndrsLast' = case tyVarBndrsTail of
                                          PlainTV x -> KindedTV x StarT
                                          _ -> tyVarBndrsTail
                      tyVarBndrs' = tyVarBndrsInit ++ [tyVarBndrsLast']
                   in ( nm
                      , tyVarBndrs'
                      , conName
                      , map (\(_, _, t) -> t) varBangTypes
                      )
            _ -> error "Expected a data or newtype declaration with one constructor"
        goDec :: Dec -> Q [Dec]
        goDec dec = do
            let (tyName, tyVarBndrs, conName, conTypes) = getInfo dec
            when (last (nameBase tyName) /= 'F') $
                error $ "Expected type name to end with 'F' - got \"" ++ nameBase tyName ++ "\""
            when (drop (length (nameBase conName) - 2) (nameBase conName) /= "F'") $
                error $ "Expected type constructor name to end with F' - got \"" ++ nameBase conName ++ "\""
            let patName, patFName :: Name
                patName = mkName $ init $ nameBase tyName
                patFName = mkName $ nameBase tyName
                rowLabel :: String
                rowLabel = case convert (init (nameBase tyName)) of
                             x:xs -> toLower x : xs
                             _ -> error "Empty patName"
                rowLabelT :: Q Type
                rowLabelT = litT (strTyLit rowLabel)
            args :: [Name] <- replicateM (length conTypes) (newName "a")
            let bndrName :: TyVarBndr -> Name
                bndrName (PlainTV n) = n
                bndrName (KindedTV n _) = n
                bndrToVar :: TyVarBndr -> Type
                bndrToVar = VarT . bndrName
                funApp :: Q Type -> Q Type -> Q Type
                funApp a b = appT (appT arrowT a) b
                conTvs :: [Type]
                conTvs = fmap bndrToVar tyVarBndrs
                appliedTyCon :: Q Type
                appliedTyCon = pure $ foldl' AppT (ConT tyName) (init conTvs)
                argsP :: [Pat]
                argsP = fmap VarP args
                appliedConExp :: Q Exp
                appliedConExp = pure $ foldl' AppE (ConE conName) (fmap VarE args)
                appliedPatF :: Q Pat
                appliedPatF = pure $ ConP patFName (fmap VarP args)
                appliedConPat :: Q Pat
                appliedConPat = pure $ ConP conName (fmap VarP args)
            r <- newName "r"
            let tvV :: Q Type
                tvV = pure $ bndrToVar (last tyVarBndrs)
                tvR :: Q Type
                tvR = varT r
                adtR :: Q Type
                adtR = [t| Fix (VarF $tvR) |]
            let patBndrsF = PlainTV r : tyVarBndrs
                patBndrs  = PlainTV r : tyVarBndrs
                patTypeCtxF = [t| (OpenAlg $tvR $rowLabelT $appliedTyCon $tvV) |]
                patTypeCtx  = [t| (OpenAlg $tvR $rowLabelT $appliedTyCon $adtR, $tvV ~ $adtR) |]
            let patRetTypeF = [t| VarF $tvR $tvV |]
            let patTypeTypeF = foldr funApp patRetTypeF (map pure conTypes)
            let patTypeType  = foldr (\x a -> do
                    x' <- x
                    v' <- tvV
                    if x' == v' then funApp adtR a else funApp x a
                    ) adtR (map pure conTypes)

            patTypeF <- forallT patBndrsF ((: []) <$> patTypeCtxF) patTypeTypeF
            patType  <- forallT patBndrs  ((: []) <$> patTypeCtx)  patTypeType

            patBody <-
              [p| (viewF (Label :: Label $rowLabelT) -> Just $appliedConPat) |]

            patClause <- [| VarF (IsJust (Label :: Label $rowLabelT) $appliedConExp) |]

            fixedPatF <- [p| Fix $appliedPatF |]

            pure [ PatSynSigD patFName patTypeF
                 , PatSynD patFName
                        (PrefixPatSyn args)
                        (ExplBidir [Clause argsP (NormalB patClause) []])
                        patBody
                 , PatSynSigD patName patType
                 , PatSynD patName (PrefixPatSyn args) ImplBidir fixedPatF
                 ]
                 

unFresh :: Name -> Name
unFresh = id
-- unFresh (Name (OccName n) _) = mkName n

