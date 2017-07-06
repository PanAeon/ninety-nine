module Ex61to69Spec(main,spec ) where

import Lib
import Test.Hspec
import Ex61to69
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Control.Monad(replicateM)
import Data.List(nub)
import System.IO.Unsafe(unsafePerformIO)


main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "99 haskell problems [61-69]" $ do
      describe "TODO:" $ do
         it "todo:" $
           True `shouldBe` True
 
