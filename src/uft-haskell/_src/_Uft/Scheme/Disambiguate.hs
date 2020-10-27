
module Uft.Scheme.Disambiguate
    ( 
    ) where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HashSet
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Uft.Primitives
import           Uft.Scheme.Ast            as In
import           Uft.UnambiguousScheme.Ast as Out

-- Used to ensure all primitives are fully saturated
etaExpand
    :: SomePrimitive
    -> Out.Exp
etaExpand p = ExpLambda args undefined -- $ ExpPrimApply p (ExpLocalVar <$> args)
    where
        args :: Vector Text
        args = undefined
        -- args = Vector.fromList $
            -- take (primArity p) $
                -- map Text.singleton ['a' .. 'z']
