module Y2024.Day5.Day5 (getDaySolutions) where
import qualified Text.Parsec.Token as Token
import qualified Data.Functor.Identity
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char (string, char, spaces, digit)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (sepBy, many1)
import Text.Parsec (parse, try, endBy)
import Control.Applicative (some)
import Text.Parsec.Combinator (sepBy1)


getDaySolutions :: (String -> String, String -> String)
getDaySolutions = (solve1, solve2)


solve1 :: String -> String
solve1 = show . parsePrintQueue

solve2 :: String -> String
solve2 = error "Second solution of day 5 not implemented yet!"


lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser emptyDef

integer :: Parser Integer
integer = read <$> many1 digit


-- | Parse a String to a PrintQueue
parsePrintQueue :: String -> PrintQueue
parsePrintQueue s = case parse printQueue "" s of
    Left e -> error $ show e
    Right r -> r


-- | Parse a PrintQueue.
printQueue :: Parser PrintQueue
printQueue = do
    constraints <- constraint `sepBy1` char '\n'
    lists <- numbers `sepBy1` char '\n'
    return $ PQ constraints lists


-- | Parse a single constraint
constraint :: Parser (Integer, Integer)
constraint = do
    l <- integer
    _ <- string "|"
    r <- integer
    return (l, r)


-- | Parse a list of numbers.
numbers :: Parser [Integer]
numbers = integer `sepBy` char ','


-- | Data structure to store a print queue.
data PrintQueue = PQ [(Integer, Integer)] [[Integer]]
    deriving Show


-- | Apply a filter on the integer lists and return only those without broken constraints.
getFilteredLists :: PrintQueue -> [[Integer]]
getFilteredLists = undefined


-- | Given a constraint, check if a list of numbers is allowed.
checkConstraints :: (Integer, Integer) -> [Integer] -> Bool
checkConstraints _ [] = True
checkConstraints (f, s) (x:xs) | s == x = f `notElem` xs && checkConstraints (f, s) xs
    | otherwise = checkConstraints (f, s) xs
