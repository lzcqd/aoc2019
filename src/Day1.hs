module Day1 (
  part1,
  part2
) where

import Lib

part1 :: IO ()
part1 = do
    f <- readFile "src/inputs/day1/input.txt"
    let r = sum (map (calculateFuel . stringToInt) (lines f))
    print r

calculateFuel :: Int -> Int
calculateFuel mass = mass `div` 3 - 2

part2 :: IO ()
part2 = do
    f <- readFile "src/inputs/day1/input.txt"
    let r = sum (map (calculateFuelRec . stringToInt) (lines f))
    print r

calculateFuelRec :: Int -> Int
calculateFuelRec mass
    | fuel <= 0 = 0
    | otherwise = fuel + calculateFuelRec fuel
    where fuel = calculateFuel mass