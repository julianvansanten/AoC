module Y2024.Day3.Day3Spec (spec) where


import Y2024.Day3.Day3 (getDaySolutions)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
    test1
    test2


sample1 :: String
sample1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"


test1 :: Spec
test1 = describe "Y2024.Day3.Day3.solve1" $ do
    it "should parse all mul's and add their results" $ do
        fst getDaySolutions sample1 `shouldBe` "161"


sample2 :: String
sample2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"


test2 :: Spec
test2 = describe "Y2024.Day3.Day3.solve2" $ do
    it "should ignore all mul's between a don't() and a do()" $ do
        snd getDaySolutions sample2 `shouldBe` "48"