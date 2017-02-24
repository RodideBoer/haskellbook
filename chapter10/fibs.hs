module Fibs where

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
fibsT x = take x fibs
fibs20 = take 20 fibs
fibsLT100 = takeWhile (<100) fibs
fact = scanl (*) 1 [1..]

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
