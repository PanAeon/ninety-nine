module HS99ListsISpec(main,spec) where

import Lib
import Test.Hspec
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Control.Monad(replicateM)

--prop_bang x = x >= 0 ==> QC.forAll (QC.ListlistLongerThan x) $ \xs ->
--  element_at xs x == xs !! x



smallNumber :: QC.Gen Int
smallNumber = ((`mod` 1000) . abs) <$> QC.suchThat QC.arbitrary (/= 0)

listLongerThan :: Int -> QC.Gen [Int]
listLongerThan x = do
  y <- fmap (+1) smallNumber
  replicateM (x+1) QC.arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "99 haskell problems" $ do
    describe "Prelude.read" $ do
      it "can parse integers" $
        read "10" `shouldBe` (10:: Int)

      it "can parse floating-point numbers" $
        read "2.5" `shouldBe` (2.5 :: Float)

    describe "99 myButLast" $ do
      it "should return one but last element" $
        myButLast [1,2,3,4,5] `shouldBe` 4

      it "should work fine on any list with size > 1" $ QC.property $ -- FIXME: $ QC.check (QC.defaultConfig { configMaxTest = 10000}) $
        QC.forAll smallNumber $ \n ->
          QC.forAll (listLongerThan n) $ \xs ->
            myButLast xs `shouldBe` (xs !! (n - 1))
