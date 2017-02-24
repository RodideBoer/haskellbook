module DividedBy where

data DividedResult a =
      Result (a, a)
    | DividedByZero
      deriving (Eq, Show)

dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy num denom
    | denom == 0 = DividedByZero
    | num < 0 = negTup $ dividedBy (-num) denom
    | denom < 0 = negTup $ dividedBy num (-denom)
    | otherwise = go num denom 0
        where negTup (Result (a, b)) = Result (negate a, b)
              negTup DividedByZero = DividedByZero
              go n d count
               | n < d = Result (count, n)
               | otherwise = go (n - d) d (count + 1)
