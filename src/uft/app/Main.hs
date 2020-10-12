{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Control.Monad.Except
import           Data.ByteString.Lazy.Lens
import           Data.Loc
import           Data.String.Conversions   (convertString)
import           Data.Text                 (Text)
import qualified Data.Text.IO as Text.IO
import           System.Environment
import           Uft.Asm.Ast
import           Uft.Asm.LabelElim
import           Uft.Asm.Parse.Lexer
import           Uft.Asm.Parse.Monad
import           Uft.Asm.Parse.Parser
import           Uft.Asm.ToVO
import qualified Uft.Asm.Parse.Tokens      as T
import qualified Uft.Lambda.Ast as Lam
import           Data.Text.Prettyprint.Doc

main :: IO ()
main = parseMain

parseMain :: IO ()
parseMain = do
    (input:_) <- getArgs
    let inputLbs = convertString input
    let pos0 = startPos "<console>"
    case runParser parseProg pos0 inputLbs of
      Left err -> print err
      Right ast -> do
          case labelElim ast of
            Left err -> print err
            Right ast' ->
                case toVO ast' of
                  Left err -> print err
                  Right vo -> Text.IO.putStrLn vo

lamMain :: IO ()
lamMain = print $ pretty $ Lam.Def "main" [] x
    where
        x :: Lam.Exp
        x = Lam.ExpLet
            [ ("foo", Lam.ExpClosure [] ["x"] $ Lam.ExpApp "print" ["x"])
            , ("one", Lam.ExpLit $ Lam.LitInt 1)
            , ("test", Lam.ExpCase "one"
                [ Lam.Alt "one" (Lam.PatNode "Cons" ["x", "xs"]) (Lam.ExpApp "print" ["x"])
                , Lam.Alt "one" Lam.PatDefault (Lam.ExpApp "print" ["one"])
                ])
            ]
            (Lam.ExpApp "foo" ["one"])

