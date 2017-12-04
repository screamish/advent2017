-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import qualified Day1

main :: IO ()
main = do
    test <- testSpec "advent2017" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    describe "day 1" $ do
      it "1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit." $ do
        Day1.captcha "1122" `shouldBe` 3
      it "1111 produces 4 because each digit (all 1) matches the next." $ do
        Day1.captcha "1111" `shouldBe` 4
      it "1234 produces 0 because no digit matches the next." $ do
        Day1.captcha "1234" `shouldBe` 0
      it "91212129 produces 9 because the only digit that matches the next one is the last digit, 9" $ do
        Day1.captcha "91212129" `shouldBe` 9
