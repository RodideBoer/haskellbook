module Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sums :: (Eq a, Num a) => a -> a
sums 1 = 1
sums x = x + sums (x - 1)

multi :: Integral a => a -> a -> a
multi x y
    | x < y = go y x
    | otherwise = go x y
        where go a 1 = a
              go a b = a + go a (b - 1)
