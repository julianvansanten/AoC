module Y2024.Day2.Day2Spec (spec) where


import Y2024.Day2.Day2 (getDaySolutions)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
    test1
    test2


sample :: String
sample = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"


test1 :: Spec
test1 = describe "Y2024.Day2.Day2.solve1" $ do
    it "should correctly see the number of incr/decr lists and correct distances" $ do
        fst getDaySolutions sample `shouldBe` "2"


test2 :: Spec
test2 = describe "Y2024.Day2.Day2.solve2" $ do
    it "should allow removal of one problematic value" $ do
        snd getDaySolutions sample `shouldBe` "4"
