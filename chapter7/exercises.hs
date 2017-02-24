module Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where (x1, _) = x `divMod` 10
          (_, d)  = x1 `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
    where xLast = x `div` 100
          d2    = xLast `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool a b c = case c of
    True -> a
    False -> b

foldBool' :: a -> a -> Bool -> a
foldBool' a b c
    | c = a
    | otherwise = b

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)
