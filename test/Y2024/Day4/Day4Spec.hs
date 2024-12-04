module Y2024.Day4.Day4Spec (spec) where


import Y2024.Day4.Day4 (getDaySolutions)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
    test1
    test2


sample :: String
sample = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"


test1 :: Spec
test1 = describe "Y2024.Day4.Day4.solve1" $ do
    it "should correctly count all occurrences of XMAS in all directions" $ do
        fst getDaySolutions sample `shouldBe` "18"


test2 :: Spec
test2 = describe "Y2024.Day4.Day4.solve2" $ do
    it "should find all MAS words in the matrix" $ do
        snd getDaySolutions sample `shouldBe` "9"
