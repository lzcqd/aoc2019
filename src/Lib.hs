module Lib
    ( stringToInt
    ) where

stringToInt :: String -> Int
stringToInt = read

replaceIdx :: [a] -> Int -> a -> [a]
replaceIdx a idx v = f ++ [v] ++ xs
                     where (f, _ : xs) = splitAt idx a