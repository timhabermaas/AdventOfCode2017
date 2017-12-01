import Day01
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day01" $ do
    describe "solution1" $ do
      it "case 1" $ do
        f1 "1122" `shouldBe` 3
      it "case 2 (multiple matches + circular list)" $ do
        f1 "1111" `shouldBe` 4
      it "case 3 (no matches)" $ do
        f1 "1234" `shouldBe` 0
      it "case 4 (cicular list)" $ do
        f1 "91212129" `shouldBe` 9
      it "solution" $ do
        f1 input1 `shouldBe` 1089
    describe "solution2" $ do
      it "case 1" $ do
        f2 "1212" `shouldBe` 6
      it "case 2" $ do
        f2 "1221" `shouldBe` 0
      it "case 3" $ do
        f2 "123425" `shouldBe` 4
      it "case 4" $ do
        f2 "123123" `shouldBe` 12
      it "case 5" $ do
        f2 "12131415" `shouldBe` 4
      it "solution" $ do
        f2 input2 `shouldBe` 1156
