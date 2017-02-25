module Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:(myIterate f (f x))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing     -> []
    Just (y, z) -> y : myUnfoldr f z

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\y -> Just (y, f y))
