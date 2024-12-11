module Y2024.Day5.Day5 (getDaySolutions, parseAndShow) where
import Util (Parser, AoCShow(..))
import Text.Parsec.Char ( string, char, digit, newline )
import Text.Parsec.Combinator ( sepBy, many1, sepEndBy1 )
import Text.Parsec (parse)
import Data.List (intercalate)
import Data.Monoid (All(..))


getDaySolutions :: (String -> String, String -> String)
getDaySolutions = (solve1, solve2)


solve1 :: String -> String
solve1 = show . sum . map (\l -> l!!(length l `div` 2)) . filterConstraints id . parsePrintQueue

solve2 :: String -> String
solve2 = show  . sum . map (\l -> l!!(length l `div` 2)) . getNumbers . fixPrintQueue . removePassing . parsePrintQueue


parseAndShow :: String -> String
parseAndShow = aocShow . parsePrintQueue


integer :: Parser Int
integer = read <$> many1 digit

-- | Parse a String to a PrintQueue
parsePrintQueue :: String -> PrintQueue
parsePrintQueue s = case parse printQueue "" s of
    Left e -> error $ show e
    Right r -> r


-- | Parse a PrintQueue.
printQueue :: Parser PrintQueue
printQueue = do
    constraints <- constraint `sepEndBy1` newline
    _ <- newline
    lists <- numbers `sepEndBy1` newline
    return $ PQ constraints lists


-- | Parse a single constraint
constraint :: Parser (Int, Int)
constraint = do
    l <- integer
    _ <- string "|"
    r <- integer
    return (l, r)


-- | Parse a list of numbers.
numbers :: Parser [Int]
numbers = integer `sepBy` char ','


-- | Data structure to store a print queue.
data PrintQueue = PQ [(Int, Int)] [[Int]]
    deriving (Eq, Show)


-- | Get the list of lists with numbers in the PrintQueue.
getNumbers :: PrintQueue -> [[Int]]
getNumbers (PQ _ xss) = xss


-- | Advent of Code show instance for PrintQueue.
instance AoCShow PrintQueue where
    aocShow (PQ cs xs) = concatMap (\(f, s) -> show f ++ "|" ++ show s ++ "\n") cs ++ "\n" ++ intercalate "\n" (map (intercalate "," . map show) xs)


-- | Apply a filter on the integer lists and return only those without broken constraints.
filterConstraints :: (Bool -> Bool) -> PrintQueue -> [[Int]]
filterConstraints f (PQ cs xss) = filter (\xs -> f (getAll (mconcat (map (\c -> All (checkConstraint c xs)) cs)))) xss


-- | Given a constraint, check if a list of numbers is allowed.
checkConstraint :: (Int, Int) -> [Int] -> Bool
checkConstraint _ [] = False
checkConstraint _ [_] = True
checkConstraint (f, s) (x:xs) | s == x = f `notElem` xs && checkConstraint (f, s) xs
    | otherwise = checkConstraint (f, s) xs


-- | Remove all lists that do not break constraints.
removePassing :: PrintQueue -> PrintQueue
removePassing pq@(PQ cs _) = PQ cs (filter (/= []) (filterConstraints not pq))


-- | Fix a single constraint on a list of numbers.
fixConstraint :: (Int, Int) -> [Int] -> [Int]
fixConstraint _ [] = []
fixConstraint _ [x] = [x]
fixConstraint (f, s) (x:xs) | s == x = f : x : filter (/= f) xs
    | otherwise = x : fixConstraint (f, s) xs


-- | For a list of constraints, filter all broken constraints for a given list.
filterBrokenConstraints :: [Int] -> [(Int, Int)] -> [(Int, Int)]
filterBrokenConstraints xs = filter (\c -> not (checkConstraint c xs))


-- | Fix a PrintQueue once.
fixOncePrintQueue :: PrintQueue -> PrintQueue
fixOncePrintQueue (PQ cs xss) = PQ cs (map (\xs -> foldr fixConstraint xs (filterBrokenConstraints xs cs)) xss)


-- | Check in an infinite list of `a`s for a fixed point.
converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys
converge _ _ = error "converge: finite list"

-- | Fix a PrintQueue until it is fixed.
fixPrintQueue :: PrintQueue -> PrintQueue
fixPrintQueue = converge (==) . iterate fixOncePrintQueue