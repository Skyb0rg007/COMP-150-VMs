
{-# LANGUAGE TemplateHaskell #-}

module Type.OpenADT.TH
    ( openADT
    , openADTDerive1
    , openADTDerive2
    ) where

import           Control.Monad
import           Data.Char                           (toLower)
import           Data.Foldable                       (foldl')
import           Control.Monad.IO.Class              (liftIO)
import           Data.Deriving
import           Data.Deriving.Internal              (unsnoc)
import           Data.Functor.Classes
import           Data.List                           (stripPrefix)
import           Data.Maybe                          (fromMaybe)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Text.Read                           (Read (..), readListPrecDefault)
import           Type.OpenADT
import           Type.VarF
import           Uft.Util                            (foldMapM)

openADT :: (String -> String) -> Q [Dec] -> Q [Dec]
openADT convert decs = do
    decs' <- decs
    foldMapM goDec decs'
    where
        getInfo :: Dec -> (Dec, Name, [TyVarBndr], Name, [BangType])
        getInfo = \case
            DataD [] (unFresh -> name) tyVarBndrs Nothing [NormalC (unFresh -> conName) bangTypes] derivClauses
              | Just (tyVarBndrsInit, tyVarBndrsTail) <- unsnoc tyVarBndrs ->
                  let tyVarBndrsLast' = case tyVarBndrsTail of
                                          PlainTV x -> KindedTV x StarT
                                          _ -> tyVarBndrsTail
                      tyVarBndrs' = tyVarBndrsInit ++ [tyVarBndrsLast']
                      derivFunctor = DerivClause (Just StockStrategy) [ConT ''Functor]
                   in ( DataD [] name tyVarBndrs' Nothing [NormalC conName bangTypes] (derivFunctor : derivClauses)
                      , name
                      , tyVarBndrs'
                      , conName
                      , bangTypes
                      )
            NewtypeD [] (unFresh -> name) tyVarBndrs Nothing (NormalC (unFresh -> conName) bangTypes) derivClauses
              | Just (tyVarBndrsInit, tyVarBndrsTail) <- unsnoc tyVarBndrs ->
                  let tyVarBndrsLast' = case tyVarBndrsTail of
                                          PlainTV x -> KindedTV x StarT
                                          _ -> tyVarBndrsTail
                      tyVarBndrs' = tyVarBndrsInit ++ [tyVarBndrsLast']
                      derivFunctor = DerivClause (Just StockStrategy) [ConT ''Functor]
                   in ( NewtypeD [] name tyVarBndrs' Nothing (NormalC conName bangTypes) (derivFunctor : derivClauses)
                      , name
                      , tyVarBndrs'
                      , conName
                      , bangTypes
                      )
            _ -> error "Expected a data or newtype declaration with one constructor"
        goDec :: Dec -> Q [Dec]
        goDec dec = do
            let (dec', tyName, tyVarBndrs, conName, bangTypes) = getInfo dec
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
            args :: [Name] <- replicateM (length bangTypes) (newName "a")
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
            let patTypeTypeF = foldr funApp patRetTypeF (map (pure . snd) bangTypes)
            let patTypeType  = foldr (\x a -> do
                    x' <- x
                    v' <- tvV
                    if x' == v' then funApp adtR a else funApp x a
                    ) adtR (map (pure . snd) bangTypes)

            patTypeF <- forallT patBndrsF ((: []) <$> patTypeCtxF) patTypeTypeF
            patType  <- forallT patBndrs  ((: []) <$> patTypeCtx)  patTypeType

            patBody <-
              [p| (viewF (Label :: Label $rowLabelT) -> Just $appliedConPat) |]

            patClause <- [| VarF (IsJust (Label :: Label $rowLabelT) $appliedConExp) |]

            fixedPatF <- [p| Fix $appliedPatF |]

            pure [ dec'
                 , PatSynSigD patFName patTypeF
                 , PatSynD patFName
                        (PrefixPatSyn args)
                        (ExplBidir [Clause argsP (NormalB patClause) []])
                        patBody
                 , PatSynSigD patName patType
                 , PatSynD patName (PrefixPatSyn args) ImplBidir fixedPatF
                 ]
                 

unFresh :: Name -> Name
unFresh (Name (OccName n) _) = mkName n

openADTDerive1 :: Name -> Q [Dec]
openADTDerive1 name =
    foldMapM ($ name)
        [ deriveShow1
        , deriveEq1
        , deriveOrd1
        , deriveShow
        , deriveEq
        , deriveOrd
        ]

openADTDerive2 :: Name -> Q [Dec]
openADTDerive2 name =
    foldMapM ($ name)
        [ deriveShow2
        , deriveEq2
        , deriveOrd2
        , deriveShow1
        , deriveEq1
        , deriveOrd1
        , deriveShow
        , deriveEq
        , deriveOrd
        ]

