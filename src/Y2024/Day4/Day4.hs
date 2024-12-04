module Y2024.Day4.Day4 (getDaySolutions) where


import Data.List (transpose)
import Data.Universe.Helpers (diagonals)


getDaySolutions :: (String -> String, String -> String)
getDaySolutions = (solve1, solve2)


solve1 :: String -> String
solve1 = show . countTotal . lines

solve2 :: String -> String
solve2 str = show neighbours
    where
        strIndices = matrixToIdList 0 (lines str)
        aLocations = filterAs strIndices
        neighbours = concatMap (`getNeighbours` strIndices) aLocations


-- | Count the total number of occurrances of XMAS in a matrix
-- TODO: add diagonals
countTotal :: [String] -> Int
countTotal xs = horizontal xs + vertical xs + diagonal1 xs + diagonal2 xs
    where
        horizontal = sum . map countHorizontal
        vertical = horizontal . transpose
        diagonal1 = horizontal . diagonals
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


-- | Convert the String matrix to a list of tuples with a Char and an x and y position.
matrixToIdList :: Int -> [String] -> [(Char, Int, Int)]
matrixToIdList _ [] = []
matrixToIdList n (x:xs) = rowIds n 0 x ++ matrixToIdList (n + 1) xs
    where
        rowIds :: Int -> Int -> String -> [(Char, Int, Int)]
        rowIds _ _ [] = []
        rowIds m o (y:ys) = (y, m, o) : rowIds m (o + 1) ys


-- | Filter all entries out of the list that are the middle of the X-MAS
filterAs :: [(Char, Int, Int)] -> [(Char, Int, Int)]
filterAs = filter (\(c, _, _) -> c == 'A')


-- | Get all relevant and neighbouring entries
getNeighbours :: (Char, Int, Int) -> [(Char, Int, Int)] -> [(Char, Int, Int)]
getNeighbours (_, x, y) = filter (\(c, i, j) -> abs (x - i) == 1 && abs (y - j) == 1 && (c == 'M' || c == 'S'))


-- | Generate possible counterparts
getCounterparts :: (Char, Int, Int) -> [(Char, Int, Int)]
getCounterparts (c, x, y) | c == 'M' = [('S', (x - 2), (y - 2)), ('S', (x + 2), (y + 2)), ('S', (x + 2), (y - 2)), ('S', (x - 2), (y + 2))]
    | c == 'S' = [('M', (x - 2), (y - 2)), ('M', (x + 2), (y + 2)), ('M', (x + 2), (y - 2)), ('M', (x - 2), (y + 2))]
    | otherwise = error "There are still entries in the list that are not either M or S"