module Main where

import Lib
import Data.List.Split
import Control.Lens

main :: IO ()
main = do
    f <- readFile "app/day2/input.txt"
    let input = map stringToInt (splitOn "," f)
    let t1 = replaceIdx input 1 12    
    let codes = replaceIdx t1 2 2
    print (execute codes 0)

execute :: [Int] -> Int -> [Int]
execute codes i
    | op == 99 = codes
    | op == 1 = execute ((element (codes !! (i+3)) .~ (codes !! (codes !! (i+1)) + codes !! (codes !! (i+2)))) codes) (i+4)
    | op == 2 = execute ((element (codes !! (i+3)) .~ (codes !! (codes !! (i+1)) * codes !! (codes !! (i+2)))) codes) (i+4) 
    | otherwise = []
    where op = codes !! i

replaceIdx :: [Int] -> Int -> Int -> [Int]
replaceIdx a idx v = (element idx .~ v) a