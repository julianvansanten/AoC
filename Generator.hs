module TemplateGenerator (main) where


import System.Directory (createDirectoryIfMissing)


main :: IO ()
main = do
    year <- getLine
    makeFolders year
    makeDayFiles year
    makeGetSolutions year
    makeSolves year
    makeSolutionsCollector year


-- Make 31 folders for each day of the week
makeFolders :: String -> IO ()
makeFolders year = mapM_ (\x -> createDirectoryIfMissing True ("./src/Y" ++ year ++ "/Day" ++ show x)) [1..31]

-- Fill each folder with a DayX.hs file
makeDayFiles :: String -> IO ()
makeDayFiles year = mapM_ (\x -> writeFile ("./src/Y" ++ year ++ "/Day" ++ show x ++ "/Day" ++ show x ++ ".hs") ("module Y" ++ year ++ ".Day" ++ show x ++ ".Day" ++ show x ++ " (getDaySolutions) where\n\n\n")) [1..31]

-- Fill each DayX.hs file with a getSolutions function that returns a tuple with two functions from String to String
makeGetSolutions :: String -> IO ()
makeGetSolutions year = mapM_ (\x -> appendFile ("./src/Y" ++ year ++ "/Day" ++ show x ++ "/Day" ++ show x ++ ".hs") ("getDaySolutions :: (String -> String, String -> String)\ngetDaySolutions = (solve1, solve2)\n\n\n")) [1..25]

makeSolves :: String -> IO ()
makeSolves year = mapM_ (\x -> appendFile ("./src/Y" ++ year ++ "/Day" ++ show x ++ "/Day" ++ show x ++ ".hs") 
        ("solve1 :: String -> String\nsolve1 = error \"First solution of day " 
        ++ show x ++ " not implemented yet!\"\n\nsolve2 :: String -> String\nsolve2 = error \"Second solution of day " ++ show x ++ " not implemented yet!\"")) [1..25]

-- Make a SolutionsCollector.hs file that takes all the getSolutions functions from each module DayX/DayX.hs and puts them in a list
makeSolutionsCollector :: String -> IO ()
makeSolutionsCollector year = do
    writeFile ("./src/Y" ++ year ++ "/SolutionsCollector.hs") ("module Y" ++ year ++ ".SolutionsCollector (getSolutions, getDay) where\n\n\n")
    mapM_ (\x -> appendFile ("./src/Y" ++ year ++ "/SolutionsCollector.hs") ("import Y" ++ year ++  ".Day" ++ show x ++ ".Day" ++ show x ++ " as Day" ++ show x ++ "\n")) [1..25]
    appendFile ("./src/Y" ++ year ++ "/SolutionsCollector.hs") "\n\n-- | List of all solutions for each day"
    appendFile ("./src/Y" ++ year ++ "/SolutionsCollector.hs") "\ngetSolutions :: [(String -> String, String -> String)]\ngetSolutions = [\n"
    mapM_ (\x -> appendFile ("./src/Y" ++ year ++ "/SolutionsCollector.hs") ("    Day" ++ show x ++ ".getDaySolutions, \n")) [1..24]
    appendFile ("./src/Y" ++ year ++ "/SolutionsCollector.hs") ("    Day" ++ show 25 ++ ".getDaySolutions\n    ]\n\n\n")
    appendFile ("./src/Y" ++ year ++ "/SolutionsCollector.hs") "-- | Get solutions for a given day\ngetDay :: Int -> (String -> String, String -> String)\ngetDay x = getSolutions !! (x - 1)\n\n"