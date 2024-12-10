module Y2024.Day7.Day7Spec (spec) where


import Test.Hspec (Spec, describe, it, shouldBe)
import Y2024.Day7.Day7 (getDaySolutions)


spec :: Spec
spec = do
    test1
    test2


sample :: String
sample = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"


test1 :: Spec
test1 = describe "Y2024.Day7.Day7.solve1" $ do
    let solve1 = fst getDaySolutions
    it "returns the correct answer for the example" $ do
        solve1 sample `shouldBe` "3749"


test2 :: Spec
test2 = describe "Y2024.Day7.Day7.solve2" $ do
    let solve2 = snd getDaySolutions
    it "is not implemented yet" $ do
        False
