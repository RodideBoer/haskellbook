module Idempotence where

import Test.QuickCheck
import Data.Char
import Data.List

idempotence :: IO ()
idempotence = do
  quickCheck $ propIdempotence capitalizeWord
  quickCheck $ propIdempotence (sort :: String -> String)

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord xs = (toUpper . head $ xs) : tail xs

propIdempotence f x =
  (f x == twice f x) &&
  (f x == fourTimes f x)
