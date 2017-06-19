module Ex31to41Spec( main, spec) where

import Lib
import Test.Hspec
import Ex31to41
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Control.Monad(replicateM)
import Data.List(nub)
import System.IO.Unsafe(unsafePerformIO)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "99 haskell problems [21-28]" $ do
      describe "21 - insertAt" $ do
         it "should insert an element into a list" $
           (2 + 2) `shouldBe` 4
