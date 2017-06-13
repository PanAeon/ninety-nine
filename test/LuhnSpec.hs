module LuhnSpec(main,spec) where

import Lib
import Test.Hspec
import qualified Luhn
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Control.Monad(replicateM)


main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Luhn" $ do
      it "should return False for invalid credit card number" $
        Luhn.check [7,9,9,2,7,3,9,8,7,1,0] `shouldBe` False

      it "should return True for the valid CC number" $
        Luhn.check [7,9,9,2,7,3,9,8,7,1,3] `shouldBe` True
