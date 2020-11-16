
module Uft.VScheme.Disambiguate
    ( disambiguate
    ) where

import           Data.Foldable     (foldl')
import           Data.Text         (Text)
import           Type.OpenADT
import           Uft.Primitives
import           Uft.UScheme.Types
import           Uft.VScheme.Types
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Sum (weaken')
import           Uft.Util

type Old =
    '[ ESetF, EVarF ]

type New =
    '[ ESetLocalF Text, ESetGlobalF
     , EVarLocalF Text, EVarGlobalF
     ]

type Req =
    '[ ELetF
     , ELetRecF
     ]

disambiguate
    :: forall r.
        ( Req :<: r
        , Old :<: r
        , Apply Functor r
        , Req :<: ((r \\ Old) ++ New)
        , Req :<: (r \\ Old)
        , Apply Functor ((r \\ Old) ++ New)
        , New :<: ((r \\ Old) ++ New)
        )
   => OpenADT r
   -> OpenADT ((r \\ Old) ++ New)
disambiguate = ($ HashSet.empty) . cata alg where
    alg :: Sum r (HashSet Text -> OpenADT ((r \\ Old) ++ New))
        -> HashSet Text
        -> OpenADT ((r \\ Old) ++ New)
    alg adt env =
        case decompose' @Old adt of
          L1 (ESetF x e)
            | x `HashSet.member` env -> ESetLocal x (e env)
            | otherwise              -> ESetGlobal x (e env)
          L1 (EVarF x)
            | x `HashSet.member` env -> EVarLocal x
            | otherwise              -> EVarGlobal x
          R1 (ELetF binds body) ->
              let env' = hashSetFromFoldable (map fst binds) <> env
               in ELet ((fmap . fmap) ($ env') binds) (body env')
          R1 (ELetRecF binds body) ->
              let env' = hashSetFromFoldable (map fst binds) <> env
               in ELet ((fmap . fmap) ($ env) binds) (body env')
          R1 x -> Fix $ fmap ($ env) $ weaken' @New x

