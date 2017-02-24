module RewritingUsingFolds where

myOr :: [Bool] -> Bool
myOr xs = foldr
          (\a b -> a || b)
          False
          xs

myOr2 :: [Bool] -> Bool
myOr2 = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f = foldr (\a -> (||) (f a)) False

myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
--myElem x = foldr (\ a b -> a == x || b) False
--myElem x = foldr (\ a -> (||) (a == x)) False
myElem x = foldr ((||) . (==) x) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x = any ((==) x)

myReverse :: [a] -> [a]
--myReverse = foldl (\ a b -> b : a ) []
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy f xs = foldl (\b a -> if f a b == GT then a else b) (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (head xs) xs
