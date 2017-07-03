module Ex54to60Spec(main,spec ) where

import Lib
import Test.Hspec
import Ex54to60
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Control.Monad(replicateM)
import Data.List(nub)
import System.IO.Unsafe(unsafePerformIO)


main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "99 haskell problems [54-60]" $ do
      describe "TODO:" $ do
         it "todo:" $
           True `shouldBe` True
