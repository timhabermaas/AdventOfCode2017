import qualified Day01 as Day01
import qualified Day02 as Day02
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day01" $ do
    describe "solution 1" $ do
      it "case 1" $ do
        Day01.f1 "1122" `shouldBe` 3
      it "case 2 (multiple matches + circular list)" $ do
        Day01.f1 "1111" `shouldBe` 4
      it "case 3 (no matches)" $ do
        Day01.f1 "1234" `shouldBe` 0
      it "case 4 (cicular list)" $ do
        Day01.f1 "91212129" `shouldBe` 9
      it "solution" $ do
        Day01.f1 Day01.input1 `shouldBe` 1089
    describe "solution 2" $ do
      it "case 1" $ do
        Day01.f2 "1212" `shouldBe` 6
      it "case 2" $ do
        Day01.f2 "1221" `shouldBe` 0
      it "case 3" $ do
        Day01.f2 "123425" `shouldBe` 4
      it "case 4" $ do
        Day01.f2 "123123" `shouldBe` 12
      it "case 5" $ do
        Day01.f2 "12131415" `shouldBe` 4
      it "solution" $ do
        Day01.f2 Day01.input2 `shouldBe` 1156
  describe "Day02" $ do
    describe "solution 1" $ do
      it "case 1" $ do
        Day02.f1 "5 1 9 5\n7 5 3\n2 4 6 8" `shouldBe` 18
    describe "solution 2" $ do
      it "case 1" $ do
        Day02.f2 "5 9 2 8\n9 4 7 3\n3 8 6 5" `shouldBe` 9
