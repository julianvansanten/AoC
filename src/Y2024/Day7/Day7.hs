module Y2024.Day7.Day7 (getDaySolutions, equation) where


import Text.Parsec
-- for emptyDef:
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity)
import Data.NumberLength (numberLength)


getDaySolutions :: (String -> String, String -> String)
getDaySolutions = (solve1, solve2)


solve1 :: String -> String
solve1 = show . sum . map fst . filter checkCombinations . parseEquations


solve2 :: String -> String
solve2 _ = show $ concatInts 123 456


type Parser = ParsecT String () Identity


def = emptyDef {
    Token.reservedOpNames = [":"]
}


lexer = Token.makeTokenParser def


symbol = Token.symbol lexer

integer :: Parser Int
integer = do
    digits <- many1 digit
    return (read digits)


parseEquations :: String -> [Equation]
parseEquations input = case parse (equation `sepEndBy1` newline) "" input of
    Left err -> error $ "parse error at " ++ show err
    Right x -> x


equation :: Parser Equation
equation = do
    n <- integer
    _ <- symbol ":"
    x <- integer `sepBy1` skipMany (char ' ')
    return (n, reverse x)


-- | An Equation is a tuple of a resulting Int and a list of Ints
type Equation = (Int, [Int])


data Operation = Mul | Add | App
    deriving (Show, Eq)


data SolutionTree = Node Operation SolutionTree SolutionTree | Leaf Int
    deriving (Show)


-- | Check if an equation is possible using multiplication and addition
checkCombinations :: Equation -> Bool
checkCombinations (_, []) = False
checkCombinations (n, [x]) = n == x
checkCombinations (n, x:xs) | n `mod` x == 0 = checkCombinations (n `div` x, xs) || checkCombinations (n - x, xs)
                            | otherwise = checkCombinations (n - x, xs)


-- | Fast Integer concatenation
concatInts :: Int -> Int -> Int
concatInts x y = x * 10 ^ numberLength y + y
