
module Language.Scheme.L0.MacroExpand.SyntaxEnv where

import           Data.Hashable          (Hashable)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Language.Scheme.L0.Ast (Name, unName)

type Env = HashMap Name Denotation

data Denotation
    = Special SpecialDenotation
    | Macro [Rule] Env
    | Identifier Name
    deriving Eq

data SpecialDenotation
    = Quote
    | Lambda
    | If
    | Set
    | Begin
    | Define
    | DefineSyntax
    | LetSyntax
    | LetRecSyntax
    | SyntaxRules
    deriving Eq

data Rule = Rule Pat Template
    deriving Eq

data Pat
    = PatVar PatternVar
    | PatSym Name
    | PatEmpty
    | PatPair Pat Pat
    | PatEllipsis EllipsisPat
    | PatVector Pat
    | PatString Text
    | PatChar Char
    | PatBool Bool
    | PatNum Double
    deriving Eq

data EllipsisPat = EllipsisPat Pat [PatternVar]
    deriving Eq

data Template
    = TempVar PatternVar
    | TempSym Name
    | TempEmpty
    | TempPair Template2 Template2
    | TempVector Template
    | TempString Text
    | TempChar Char
    | TempBool Bool
    | TempNum Double
    deriving Eq

data Template2
    = Temp2Temp Template
    | Temp2Ellipsis EllipsisTemp
    deriving Eq

data EllipsisTemp = EllipsisTemp Template [Name]
    deriving Eq

data PatternVar = PatternVar Name Int
    deriving (Eq, Generic, Hashable)

instance Show PatternVar where
    show (PatternVar n _) = show $ unName n

lookup :: Name -> Env -> Denotation
lookup name env = fromMaybe (Identifier name) (HashMap.lookup name env)

