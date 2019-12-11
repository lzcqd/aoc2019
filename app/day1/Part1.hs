module Main where

import Lib

main :: IO ()
main = do
    f <- readFile "app/day1/input.txt"
    let r = sum (map (calculateFuel . stringToInt) (lines f))
    print r

calculateFuel :: Int -> Int
calculateFuel mass = mass `div` 3 - 2