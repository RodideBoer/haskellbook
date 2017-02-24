module MC91 where

mc91 :: (Num a, Ord a) => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ x + 11
