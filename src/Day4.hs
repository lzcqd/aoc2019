module Day4 (
    part1,
    part2
) where

part1 :: IO ()
part1 =
   print $ length $ filter (\i -> hasDouble i && onlyIncrease i) $ map toDigits [147981..691423]

part2 :: IO ()
part2 = print $
            length $
                filter (\i -> hasDouble2 i && onlyIncrease i) $ map toDigits [147981..691423]

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits i = toDigits (i `div` 10) ++ [i `mod` 10]

hasDouble :: [Int] -> Bool
hasDouble [] = False
hasDouble [_] = False
hasDouble (x:y:xs)
    | x == y = True
    | otherwise = hasDouble (y:xs)

hasDouble2 :: [Int] -> Bool
hasDouble2 [] = False
hasDouble2 [_] = False
hasDouble2 [x,y] = x == y
hasDouble2 (x:y:z:xs)
    | x == y && x /= z = True
    | x == y && x == z = hasDouble2 (remove x xs)
    | otherwise = hasDouble2 (y:z:xs)

remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove i (x:xs)
    | i == x = remove i xs
    | otherwise = x:xs

onlyIncrease :: [Int] -> Bool
onlyIncrease [] = True
onlyIncrease [_] = True
onlyIncrease (x:y:xs)
    | x <= y = onlyIncrease (y:xs)
    | otherwise = False