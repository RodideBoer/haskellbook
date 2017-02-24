module Exercises where

import           Data.Char

filterUpper :: String -> String
filterUpper xs = [x | x <- xs, isUpper x]

filterUpper2 :: String -> String
filterUpper2 xs = filter (isUpper) xs

cap :: String -> String
cap []     = []
cap (x:xs) = (toUpper x) : xs

cap2 :: String -> String
cap2 xs = (toUpper . head $ xs) : tail xs

capAll :: String -> String
capAll []     = []
capAll (x:xs) = (toUpper x) : capAll xs

capFirst :: String -> Char
capFirst xs = toUpper . head $ xs

capFirstPointFree :: String -> Char
capFirstPointFree = toUpper . head

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = x == y || myElem x ys

myAnyElem :: Eq a => a -> [a] -> Bool
myAnyElem x ys = myAny (==x) ys

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []       = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ map f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []        = undefined
myMaximumBy _ (x:[])    = x
myMaximumBy f (x:x':xs) =
    if f x x' == GT
    then myMaximumBy f (x:xs)
    else myMaximumBy f (x':xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []        = undefined
myMinimumBy _ (x:[])    = x
myMinimumBy f (x:x':xs) =
    if f x x' == LT
    then myMinimumBy f (x:xs)
    else myMinimumBy f (x':xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
