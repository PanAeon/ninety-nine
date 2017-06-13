module Ex11to20Spec( main, spec) where

import Lib
import Test.Hspec
import Ex11to20
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Control.Monad(replicateM)


main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "99 haskell problems [11-20]" $ do
      describe "11 - encodeModified" $ do
         it "should return [] on []" $
           encodeModified ([]::[String]) `shouldBe` []

         it "encode non-empty list" $
           encodeModified [1,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,7]
             `shouldBe` [Single 1,Multiple 2 2,Multiple 3 3,Multiple 3 4,Multiple 4 5,Multiple 6 6,Single 7]

      describe "12 - decodeModifed" $ do
        it "should decode []" $ do
          decodeModified ([]::[MList Int]) `shouldBe` []
        it "should decode encoded list" $ do
          decodeModified [Single 1,Multiple 2 2,Multiple 3 3,Multiple 3 4,Multiple 4 5,Multiple 6 6,Single 7]
            `shouldBe` [1,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,7]

      describe "13 - encodeDirect" $ do
           it "should return [] on []" $
             encodeDirect ([]::[String]) `shouldBe` []

           it "encode non-empty list" $
             encodeDirect [1,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,7]
               `shouldBe` [Single 1,Multiple 2 2,Multiple 3 3,Multiple 3 4,Multiple 4 5,Multiple 6 6,Single 7]

      describe "14 - dupli" $ do
        it "should dupli []" $
          dupli ([]::[String]) `shouldBe` []

        it "should dupli []" $
          dupli ([1,2,3]) `shouldBe` [1,1,2,2,3,3]

      describe "15 - repli" $ do
       it "should repli []" $
         repli 2 ([]::[String]) `shouldBe` []

       it "should repli 1 xs" $
           repli 1 [1,2,3] `shouldBe` [1,2,3]

       it "should repli 0 to []" $
               repli 0 [1,2,3] `shouldBe` []

       it "should repli n " $
          repli 3 ([1,2,3]) `shouldBe` [1,1,1,2,2,2,3,3,3]
