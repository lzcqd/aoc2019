module Main where

import System.Environment
import Day1
import Day2
import Day3

main :: IO()
main = do
    args <- getArgs
    run (head args)

run :: String -> IO()
run arg
    | arg == "d1p1" = Day1.part1
    | arg == "d1p2" = Day1.part2
    | arg == "d2p1" = Day2.part1
    | arg == "d2p2" = Day2.part2
    | arg == "d3p1" = Day3.part1
    | otherwise = putStr "not yet done"