
module Uft.Asm.Parse
    ( parseAsm
    ) where

import           Data.Loc                (startPos)
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           Uft.Asm.Ast             (Prog)
import           Uft.Asm.Parse.Monad     (runParser)
import           Uft.Asm.Parse.Parser    (parseProg)

parseAsm :: FilePath -> Text -> Either Text Prog
parseAsm file input =
     runParser parseProg (startPos file) (convertString input)

