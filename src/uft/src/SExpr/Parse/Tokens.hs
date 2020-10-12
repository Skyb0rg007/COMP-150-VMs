
module SExpr.Parse.Tokens
    ( Token
    , Token_ (..)
    ) where

import           Data.Loc
import           Data.Text (Text)

type Token = L Token_

data Token_
    = LParen
    | RParen
    | Dot
    | EOF

