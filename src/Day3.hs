module Day3 (
    part1,
    part2
) where

import Lib
import Data.List.Split
import qualified Data.Map.Strict as Map

type Point = (Int, Int)
part1 :: IO ()
part1 = do
    f <- readFile "src/inputs/day3/input.txt"
    let [w1, w2] = lines f
    let m1 = getMap (splitOn "," w1) (0,0) 1
    let m2 = getMap (splitOn "," w2) (0,0) 1
    let intersects = Map.keys (Map.intersection m1 m2)
    print (minimum (map (\p -> abs (fst p) + abs (snd p)) intersects))
 
part2 :: IO ()
part2 = do
    f <- readFile "src/inputs/day3/input.txt"
    let [w1, w2] = lines f
    let m1 = getMap (splitOn "," w1) (0,0) 1
    let m2 = getMap (splitOn "," w2) (0,0) 1
    print (minimum (Map.elems (Map.intersectionWith (+) m1 m2)))

getMap :: [String] -> Point -> Int -> Map.Map Point Int
getMap [] _ _ = Map.empty
getMap (x:xs) start stepCount = let p = zip (getP x start) [stepCount..] 
                                    m = getMap xs (fst (last p)) (stepCount + length p)
                                    in Map.union (Map.fromList p) m 

getP :: String -> Point -> [Point]
getP step p 
    | head step == 'U' = generateP count p up 
    | head step == 'D' = generateP count p down
    | head step == 'R' = generateP count p right
    | head step == 'L' = generateP count p left  
    | otherwise = []
    where count = stringToInt (tail step)
          
generateP :: Int -> Point -> (Point -> Point) -> [Point]
generateP remaining currP move
    | remaining == 0 = []
    | otherwise = move currP : generateP (remaining-1) (move currP) move

up :: Point -> Point
up (x,y) = (x,y+1)

down :: Point -> Point
down (x,y) = (x,y-1)

right :: Point -> Point
right (x,y) = (x+1,y)

left :: Point -> Point
left (x,y) = (x-1,y)