
module Uft.VScheme.PrimPrelude
    ( primPrelude
    ) where

import           Data.Functor      ((<&>))
import           Type.OpenADT
import           Uft.Primitives
import           Uft.VScheme.Types

primPrelude :: ('[EPrimApplyF, DDefineF, EVarF, EApplyF] :<: r)
            => [OpenADT r]
primPrelude =
    exportedPrims <&> \name ->
        let p = prim name
            args = mkArgs (_prim_arity p)
         in DDefine name args $ EPrimApply p (map EVar args)
    where
        exportedPrims =
            [ "*"
            , "+"
            , "-"
            , "/"
            , "//"
            , "append"
            , "cons"
            , "hash"
            , "list->vector"
            , "print"
            , "println"
            , "printu"
            ]
        mkArgs n = drop (3 - n) ["a", "b", "c"]

