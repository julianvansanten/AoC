module Y2024.Day2.Day2 (getDaySolutions) where


getDaySolutions :: (String -> String, String -> String)
getDaySolutions = (solve1, solve2)


solve1 :: String -> String
solve1 = show . length . filter (isSafe 0) . map (map (read :: String -> Int) . words) . lines


solve2 :: String -> String
solve2 = show . length . filter (isSafe 1) . map (map (read :: String -> Int) . words) . lines


isSafe :: Int -> [Int] -> Bool
isSafe (-1) _ = False
isSafe _ [] = True
isSafe _ [_] = True
isSafe n l@(_:xs) = decreasing n l || increasing n l || isSafe (n - 1) xs


decreasing :: Int -> [Int] -> Bool
decreasing _ [] = True
decreasing _ [_] = True
decreasing n (x:y:xs) | n == 0 = x > y && abs (x - y) <= 3 && decreasing 0 (y:xs)
    | otherwise = (x > y && abs (x - y) <= 3 && decreasing n (y:xs)) || decreasing (n - 1) (x:xs)


increasing :: Int -> [Int] -> Bool
increasing _ [] = True
increasing _ [_] = True
increasing n (x:y:xs) | n == 0 = x < y && abs (x - y) <= 3 && increasing 0 (y:xs)
    | otherwise = (x < y && abs (x - y) <= 3 && increasing n (y:xs)) || increasing (n - 1) (x:xs)
