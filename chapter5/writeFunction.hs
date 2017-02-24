module WriteFunction where

i :: a -> a
i x = x

c :: a -> b -> a
c a _ = a

c'' :: b -> a -> b
c'' a _ = a

c' :: a -> b -> b
c' _ b = b

r :: [a] -> [a]
r as = as

r' :: [a] -> [a]
r' [] = []
r' as = tail as

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

fa :: (a -> c) -> a -> a
fa _ a = a

a' :: (a -> b) -> a -> b
a' aToB a = aToB a
