module Main (main) where


import System.Environment (getArgs)
import Y2024.SolutionsCollector (getDay)

-- | Main function
main :: IO ()
main = do
    day <- getDayFromArgs
    filepath <- getInputFilePathFromArgs
    let (solve1, solve2) = getDay day
    input <- readFile filepath
    putStrLn "Solution of the first question:"
    putStrLn $ solve1 input
    putStrLn "Solution of the second question:"
    putStrLn $ solve2 input


-- | If the day is the first program argument, return it. Otherwise, ask the user for a day and return the integer value of the day
getDayFromArgs :: IO Int
getDayFromArgs = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Please enter a day: "
            read <$> getLine
        (day:_) -> return $ read day


-- | If the file path is the second program argument, return it. Otherwise, ask the user for a file path to the input and return it as a filepath
getInputFilePathFromArgs :: IO FilePath
getInputFilePathFromArgs = do
    args <- getArgs
    if length args > 1 then
        return $ args !! 1
    else do
        putStrLn "Please enter the path to the input file: "
        getLine