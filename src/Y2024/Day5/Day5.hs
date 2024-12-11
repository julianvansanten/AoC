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
solve1 = show . sum . map (\l -> l!!(length l `div` 2)) . filterConstraints . parsePrintQueue

solve2 :: String -> String
solve2 = error "Second solution of day 5 not implemented yet!"


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
    deriving Show


-- | Advent of Code show instance for PrintQueue.
instance AoCShow PrintQueue where
    aocShow (PQ cs xs) = concatMap (\(f, s) -> show f ++ "|" ++ show s ++ "\n") cs ++ "\n" ++ intercalate "\n" (map (intercalate "," . map show) xs)


-- | Apply a filter on the integer lists and return only those without broken constraints.
filterConstraints :: PrintQueue -> [[Int]]
filterConstraints (PQ cs xss) = filter (\xs -> getAll (mconcat (map (\c -> All (checkConstraint c xs)) cs))) xss


-- | Given a constraint, check if a list of numbers is allowed.
checkConstraint :: (Int, Int) -> [Int] -> Bool
checkConstraint _ [] = False
checkConstraint _ [_] = True
checkConstraint (f, s) (x:xs) | s == x = f `notElem` xs && checkConstraint (f, s) xs
    | otherwise = checkConstraint (f, s) xs
