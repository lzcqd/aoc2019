module Day2 (
    part1,
    part2
) where

import Lib
import Data.List.Split

part1 :: IO ()
part1 = do
    f <- readFile "src/inputs/day2/input.txt"
    let input = map stringToInt (splitOn "," f)
    let t1 = replaceIdx input 1 12    
    let codes = replaceIdx t1 2 2
    print (execute codes 0)

part2 :: IO ()
part2 = do
    f <- readFile "src/inputs/day2/input.txt"
    let input = map stringToInt (splitOn "," f)
    let r = find (generateResult input)
    print r
    print (100 * (r !! 1) + (r !! 2))

execute :: [Int] -> Int -> [Int]
execute codes i
    | op == 99 = codes
    | op == 1 = compute (+)
    | op == 2 = compute (*) 
    | otherwise = []
    where op = codes !! i
          param idx = codes !! (codes !! idx)
          binaryOp f = replaceIdx codes (codes !! (i+3)) (f (param (i+1)) (param (i+2)))
          compute g = execute (binaryOp g) (i+4)

find :: [[Int]] -> [Int]
find results
    | head (head results) == 19690720 = head results
    | otherwise = find (tail results) 

generateResult :: [Int] -> [[Int]]
generateResult codes = [execute (replaceIdx (replaceIdx codes 1 n) 2 v) 0 | n <- [0..99], v <- [0..99]]