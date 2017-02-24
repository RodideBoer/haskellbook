module Filtering where

multiplesOfThree :: Integral a => [a] -> [a]
multiplesOfThree xs = filter (\x -> (mod x 3) == 0) xs

countMultiplesOfThree :: Integral a => [a] -> Int
countMultiplesOfThree xs = length . multiplesOfThree $ xs

removeArticles :: String -> [String]
removeArticles s = filter (/= "an") . filter (/= "a") . filter (/= "the") . words $ s
