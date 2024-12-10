module Y2024.Day9.Day9Spec (spec) where


import Test.Hspec (Spec, describe, it, shouldBe)

import Y2024.Day9.Day9 (getDaySolutions)


spec :: Spec
spec = do
    test1
    test2


sample :: String
sample = "2333133121414131402"


test1 :: Spec
test1 = describe "Y2024.Day9.Day9.solve1" $ do
    let solve1 = fst getDaySolutions
    it "should correctly return the sum of possible mul/add combinations" $ do
        solve1 sample `shouldBe` "1928"


test2 :: Spec
test2 = describe "Y2024.Day9.Day9.solve2" $ do
    let solve2 = snd getDaySolutions
    it "is not implemented yet" $ do
        False