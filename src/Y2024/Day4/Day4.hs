module Y2024.Day4.Day4 (getDaySolutions) where


import Data.List (transpose)
import Data.Universe.Helpers (diagonals)


getDaySolutions :: (String -> String, String -> String)
getDaySolutions = (solve1, solve2)


solve1 :: String -> String
solve1 = show . countTotal . lines

solve2 :: String -> String
solve2 = error "Second solution of day 4 not implemented yet!"


-- | Count the total number of occurrances of XMAS in a matrix
-- TODO: add diagonals
countTotal :: [String] -> Int
countTotal xs = horizontal xs + vertical xs + diagonal1 xs + diagonal2 xs
    where
        horizontal = sum . map countHorizontal
        vertical = horizontal . transpose
        diagonal1 ys = sum (map (countXmas . reverse) (diagonals ys)) + sum (map countXmas (diagonals ys))
        diagonal2 = diagonal1 . transpose . map reverse


-- | Count the number of occurrances of XMAS and SAMX in a String
countHorizontal :: String -> Int
countHorizontal xs = countXmas xs + countXmas (reverse xs)


-- | Count the number of occurrances of XMAS in a String
countXmas :: String -> Int
countXmas [] = 0
countXmas [_] = 0
countXmas [_,_] = 0
countXmas [_,_,_] = 0
countXmas ('X':'M':'A':'S':xs) = 1 + countXmas xs
countXmas (_:xs) = countXmas xs
