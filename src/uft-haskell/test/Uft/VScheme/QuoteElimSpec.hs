
module Uft.VScheme.QuoteElimSpec (main, spec) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Test.Hspec
import           Test.QuickCheck
import           Uft.Parse
import           Uft.SExpr.Parse
import           Uft.VScheme.QuoteElim
import           Uft.VScheme.Types
import           Uft.Primitives

main :: IO ()
main = hspec spec

err :: (MonadFail m, Show a) => Either a b -> m b
err (Left err) = error (show err)
err (Right x) = pure x

spec :: Spec
spec =
    describe "Quote elimination works" $ do
        it "Eliminates unquote-splicing properly" $ do
            let input = "`(1 ,@2 3)"
            sexp <- err $ parseSExpr "" input
            vscheme <- err $ parseFEither @VSchemeR PCTop sexp
            quoteElim vscheme `shouldBe`
              EPrimApply (prim "cons")
              [ LNum 1
              , EPrimApply (prim "append")
                [ LNum 2
                , EPrimApply (prim "cons")
                  [ LNum 3
                  , LEmpty
                  ]
                ]
              ]
