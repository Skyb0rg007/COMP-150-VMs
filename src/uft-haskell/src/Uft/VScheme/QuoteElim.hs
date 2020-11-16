
module Uft.VScheme.QuoteElim
    ( quoteElim
    ) where

import           Data.Foldable        (foldl')
import           Type.OpenADT
import           Uft.Primitives
import           Uft.VScheme.Types
-- import           Debug.Trace
import           Data.Functor.Classes

type Old =
    '[ EQuoteF
     , EQuasiF
     , EUnquoteF
     , EUnquoteSplicingF
     , LPairF
     , LVectorF
     ]

type Req =
    '[ EPrimApplyF
     , LEmptyF
     ]

quoteElim
    :: forall r.
        ( Req :<: (r \\ Old)
        , Old :<: r, Apply Functor r
        , Apply Functor (r \\ Old)
        )
    => OpenADT r
    -> OpenADT (r \\ Old)
quoteElim = para alg where
    alg :: Sum r (OpenADT r, OpenADT (r \\ Old)) -> OpenADT (r \\ Old)
    alg adt =
        case decompose' @Old adt of
          L1 (EQuoteF (_, x)) -> x
          L1 (EQuasiF (_, x)) -> x
          L1 (EUnquoteF (_, x)) -> x
          L1 (EUnquoteSplicingF (_, x)) -> x

          L1 (LPairF (EUnquoteSplicing{}, a) (_, LEmpty)) -> a
          L1 (LPairF (EUnquoteSplicing{}, a) (_, b)) -> EPrimApply (prim "append") [a, b]
          L1 (LPairF (_, a) (_, b)) -> EPrimApply (prim "cons") [a, b]
          L1 (LVectorF es) ->
              let f (EUnquoteSplicing{}, x) acc = EPrimApply (prim "append") [x, acc]
                  f (_, x) acc                  = EPrimApply (prim "cons") [x, acc]
               in EPrimApply (prim "list->vector") [foldr f LEmpty es]

          R1 x -> Fix (fmap snd x)

