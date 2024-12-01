module Y2024.Day1.Day1 (getDaySolutions) where

import Data.List (sort, transpose)

getDaySolutions :: (String -> String, String -> String)
getDaySolutions = (solve1, solve2)

solve1 :: String -> String
solve1 = show . sum . (\l -> zipWith (\x y -> abs (x - y)) (head l) (head (tail l))) . map sort . getLists

solve2 :: String -> String
solve2 s = show $ sum $ map (\n -> count n secondList * n) firstList
    where 
        lst = getLists s
        firstList = head lst
        secondList = head (tail lst)

getLists :: String -> [[Int]]
getLists = transpose . map (map (read :: String -> Int) . words) . lines

count :: Eq a => a -> [a] -> Int
count find xs = length (filter (== find) xs)
