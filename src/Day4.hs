module Day4 (
    part1
) where

part1 :: IO ()
part1 =
   print $ length $ filter (\i -> hasDouble i && onlyIncrease i) $ map toDigits [147981..691423]

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits i = toDigits (i `div` 10) ++ [i `mod` 10]

hasDouble :: [Int] -> Bool
hasDouble [] = False
hasDouble [_] = False
hasDouble (x:y:xs)
    | x == y = True
    | otherwise = hasDouble (y:xs)

onlyIncrease :: [Int] -> Bool
onlyIncrease [] = True
onlyIncrease [_] = True
onlyIncrease (x:y:xs)
    | x <= y = onlyIncrease (y:xs)
    | otherwise = False