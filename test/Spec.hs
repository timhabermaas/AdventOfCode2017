import qualified Day01 as Day01
import qualified Day02 as Day02
import qualified Day03 as Day03
import qualified Day04 as Day04
import qualified Day05 as Day05
import qualified Day06 as Day06
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

  describe "Day03" $ do
    describe "solution 1" $ do
      it "case 1" $ do
        Day03.f1 1 `shouldBe` 0
      it "solution" $ do
        Day03.f1 289326 `shouldBe` 419
    describe "solution 2" $ do
      it "solution" $ do
        Day03.f2 289326 `shouldBe` 295229

  describe "Day04" $ do
    describe "solution 1" $ do
      it "cases" $ do
        Day04.f1 "aa bb cc dd ee" `shouldBe` 1
        Day04.f1 "aa bb cc dd aa" `shouldBe` 0
        Day04.f1 "aa bb cc dd aaa" `shouldBe` 1
    describe "solution 2" $ do
      it "cases" $ do
        Day04.f2 "abcde fghij" `shouldBe` 1
        Day04.f2 "abcde xyz ecdab" `shouldBe` 0
        Day04.f2 "a ab abc abd abf abj" `shouldBe` 1

  describe "Day05" $ do
    describe "solution 1" $ do
      it "cases" $ do
        Day05.f1 "0\n3\n0\n1\n-3" `shouldBe` 5
    describe "solution 2" $ do
      it "cases" $ do
        Day05.f2 "0\n3\n0\n1\n-3" `shouldBe` 10

  describe "Day06" $ do
    describe "solution 1" $ do
      it "cases" $ do
        Day06.f1 "0 2 7 0" `shouldBe` 5
    describe "solution 2" $ do
      it "cases" $ do
        Day06.f2 "0 2 7 0" `shouldBe` 4
