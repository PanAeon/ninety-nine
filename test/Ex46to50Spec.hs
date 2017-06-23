module Ex46to50Spec(main,spec ) where

import Lib
import Test.Hspec
import Ex46to50
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Control.Monad(replicateM)
import Data.List(nub)
import System.IO.Unsafe(unsafePerformIO)


main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "99 haskell problems [46-50]" $ do
      describe "TODO:" $ do
         it "todo:" $
           True `shouldBe` True
