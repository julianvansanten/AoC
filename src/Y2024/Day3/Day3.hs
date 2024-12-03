module Y2024.Day3.Day3 (getDaySolutions) where
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import qualified Data.Functor.Identity
import Text.Parsec.String (Parser)
import Control.Applicative (Alternative(..))
import Text.Parsec (parse)
import Text.Parsec.Prim (try)
import Text.Parsec.Char (string, anyChar)


getDaySolutions :: (String -> String, String -> String)
getDaySolutions = (solve1, solve2)


solve1 :: String -> String
solve1 = show . sum . map eval . parseOperations


solve2 :: String -> String
solve2 = error "Second solution of day 3 not implemented yet!"

sample :: String
sample = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"


lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser emptyDef
integer = Token.integer lexer


parseOperations :: String -> [Operation]
parseOperations s = case parse operations "" s of
    Left e -> error $ show e
    Right r -> r


-- | Parse a string into a list of operations.
-- Everything that is not a mul with two integers is ignored.
-- Example: `parseOperations "%^&mul(2,4)xxxmul(3,7)do_not_mul(5,5)mul(32,64)mul(11,8)mul(8,5)"` returns `[Mul 2 4, Mul 3 7, Mul 32 64, Mul 11 8, Mul 8 5]`
operations :: Parser [Operation]
operations = many (try op <|> (anyChar >> return (Mul 0 0))) -- try is needed to backtrack if the first parser fails



-- | Parse a single mul into an operation
op :: Parser Operation
op = do
    _ <- string "mul("
    l <- integer
    _ <- string ","
    r <- integer
    _ <- string ")"
    return $ Mul l r

-- | An operation is only a Mul for the first exercise
data Operation = Mul Integer Integer
    deriving (Eq, Show)


-- | Evaluate a parsed operation
eval :: Operation -> Integer
eval (Mul l r) = l * r