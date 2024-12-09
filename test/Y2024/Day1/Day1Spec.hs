module Y2024.Day1.Day1Spec (spec) where


import Y2024.Day1.Day1 (getDaySolutions)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
    test1
    test2
    

sample :: String
sample = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
    
test1 :: Spec
test1 = describe "Y2024.Day1.Day1.solve1" $ do
    it "should sort both lists, pair the values and sum the differences" $ do
        fst getDaySolutions sample `shouldBe` "11"


test2 :: Spec
test2 = describe "Y2024.Day1.Day1.solve2" $ do
    it "should, for each number in the left list, count each occurrence of a number in the second list and sum the total" $ do
        snd getDaySolutions sample `shouldBe` "31"