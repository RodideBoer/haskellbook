module AsPatterns where

import Data.Char

-- This version without as-patterns just match each element of first list in second list
hasAllElementsIn :: (Eq a) => [a] -> [a] -> Bool
hasAllElementsIn [] _ = True
hasAllElementsIn _ [] = False
hasAllElementsIn (x:xs) ys = elem x ys && isSubsequenceOf xs ys

-- This version with as-patterns matches [1,1,1,2] in [1,2,3]
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xxs@(x:xs) yys@(y:ys)
    | x == y    = isSubsequenceOf xs yys
    | otherwise = isSubsequenceOf xxs ys

-- To match each element seperately, even if already found, eg: [1,1,1,2] in [1,1,1,2,3] then use this:
isSubsequenceOfEach :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOfEach [] _ = True
isSubsequenceOfEach _ [] = False
isSubsequenceOfEach xxs@(x:xs) (y:ys)
    | x == y    = isSubsequenceOfEach xs ys
    | otherwise = isSubsequenceOfEach xxs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords xs = capitalizeToTuple (takeWhile (/=' ') xs) : capitalizeWords (dropWhile (==' ') (dropWhile (/=' ') xs))
    where capitalizeToTuple []         = ([], [])
          capitalizeToTuple yys@(y:ys) = (toUpper y : ys, yys)

toWords :: String -> [String]
toWords [] = []
toWords xs = takeWhile (/=' ') xs : toWords (dropWhile (==' ') (dropWhile (/=' ') xs))
