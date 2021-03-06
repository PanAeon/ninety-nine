module Ex21to28Spec( main, spec) where

import Lib
import Test.Hspec
import Ex21to28
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
           insertAt' 2 'X' "abcd" `shouldBe` "aXbcd"

      describe "22 - range" $ do
        it "should produce valid range" $
          range' 4 9 `shouldBe` [4,5,6,7,8,9]
 -- TODO: works, but better to eliminate unsafePerformIO somehow
      describe "24 - diffSelect" $ do
        it "should produce distinct numbers" $
          (unsafePerformIO $ diffSelect 6 49) `shouldSatisfy` ((==) <$> length <*> (length . nub))
