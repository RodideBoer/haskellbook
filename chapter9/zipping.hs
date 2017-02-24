module Zipping where

myZip :: [a] -> [b] -> [(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip xs ys = (head xs, head ys) : myZip (tail xs) (tail ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f xs ys = f (head xs) (head ys) : myZipWith f (tail xs) (tail ys)

createTuple :: a -> b -> (a,b)
createTuple a b = (a,b)

myEasyZip :: [a] -> [b] -> [(a,b)]
myEasyZip xs ys = myZipWith createTuple xs ys
