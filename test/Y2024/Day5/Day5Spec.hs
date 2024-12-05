module Y2024.Day5.Day5Spec (spec) where


import Y2024.Day5.Day5 (getDaySolutions)
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
    test1 
    test2


sample :: String
sample = "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"


test1 :: Spec
test1 = describe "Y2024.Day5.Day5.solve1" $ do
    it "should do something" $ do
        fst getDaySolutions sample `shouldBe` "143"


test2 :: Spec
test2 = describe "Y2024.Day5.Day5.solve2" $ do
    it "is not implemented yet" False