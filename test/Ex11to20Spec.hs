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
      describe "16 - drop nth" $ do
        it "should drop []" $
          drop' 3 ([]::[String]) `shouldBe` []
        it "should drop nth eleme" $
          drop' 3 [1,2,3,4,5,6] `shouldBe` [1,2,4,5]

      describe "17 - split " $  do
        it "should split list at n" $
          split 3 [1,2,3,4,5,6] `shouldBe` ([1,2,3], [4,5,6])
        it "should split list at 1" $
          split 1 [1,2,3,4,5,6] `shouldBe` ([1], [2,3,4,5,6])
        it "should split empty list at n" $
          split 3 ([]::[String]) `shouldBe` ([], [])
        it "should split at n > length xs" $
          split 100 [1,2,3,4,5,6] `shouldBe` ([1,2,3,4,5,6], [])

      describe "18 - slice" $ do
        it "should slice list " $
          slice 2 5 [1,2,3,4,5,6,7,8,9,0] `shouldBe` [2,3,4,5]

      describe "19 - rotate" $ do
        it "should rotate +3" $
          rotate 3 [1,2,3,4,5,6] `shouldBe` [4,5,6,1,2,3]
        it "should rotate +9" $
          rotate 3 [1,2,3,4,5,6] `shouldBe` [4,5,6,1,2,3]
        it "should rotate -3" $
          rotate (-3) [1,2,3,4,5,6] `shouldBe` [4,5,6,1,2,3]

      describe "20 - delete kth element" $ do
        it  "should delete kth element from list" $
          removeAt' 3 [1,2,3,4,5,6] `shouldBe` (3, [1,2,4,5,6])
