module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord = concat . intersperse "-" . map wordNumber . digits

digits :: Int -> [Int]
digits n
  | n <= 0 = []
  | otherwise = digits (div n 10) ++ [mod n 10]

wordNumber :: Int -> String
wordNumber 0 = "zero"
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber 4 = "four"
wordNumber 5 = "five"
wordNumber 6 = "six"
wordNumber 7 = "seven"
wordNumber 8 = "eight"
wordNumber 9 = "nine"
wordNumber _ = "nodigit"
