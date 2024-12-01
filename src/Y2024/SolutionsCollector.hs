module Y2024.SolutionsCollector (getSolutions, getDay) where


import Y2024.Day1.Day1 as Day1
import Y2024.Day2.Day2 as Day2
import Y2024.Day3.Day3 as Day3
import Y2024.Day4.Day4 as Day4
import Y2024.Day5.Day5 as Day5
import Y2024.Day6.Day6 as Day6
import Y2024.Day7.Day7 as Day7
import Y2024.Day8.Day8 as Day8
import Y2024.Day9.Day9 as Day9
import Y2024.Day10.Day10 as Day10
import Y2024.Day11.Day11 as Day11
import Y2024.Day12.Day12 as Day12
import Y2024.Day13.Day13 as Day13
import Y2024.Day14.Day14 as Day14
import Y2024.Day15.Day15 as Day15
import Y2024.Day16.Day16 as Day16
import Y2024.Day17.Day17 as Day17
import Y2024.Day18.Day18 as Day18
import Y2024.Day19.Day19 as Day19
import Y2024.Day20.Day20 as Day20
import Y2024.Day21.Day21 as Day21
import Y2024.Day22.Day22 as Day22
import Y2024.Day23.Day23 as Day23
import Y2024.Day24.Day24 as Day24
import Y2024.Day25.Day25 as Day25


-- | List of all solutions for each day
getSolutions :: [(String -> String, String -> String)]
getSolutions = [
    Day1.getDaySolutions, 
    Day2.getDaySolutions, 
    Day3.getDaySolutions, 
    Day4.getDaySolutions, 
    Day5.getDaySolutions, 
    Day6.getDaySolutions, 
    Day7.getDaySolutions, 
    Day8.getDaySolutions, 
    Day9.getDaySolutions, 
    Day10.getDaySolutions, 
    Day11.getDaySolutions, 
    Day12.getDaySolutions, 
    Day13.getDaySolutions, 
    Day14.getDaySolutions, 
    Day15.getDaySolutions, 
    Day16.getDaySolutions, 
    Day17.getDaySolutions, 
    Day18.getDaySolutions, 
    Day19.getDaySolutions, 
    Day20.getDaySolutions, 
    Day21.getDaySolutions, 
    Day22.getDaySolutions, 
    Day23.getDaySolutions, 
    Day24.getDaySolutions, 
    Day25.getDaySolutions
    ]


-- | Get solutions for a given day
getDay :: Int -> (String -> String, String -> String)
getDay x = getSolutions !! (x - 1)

