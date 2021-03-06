{-# LANGUAGE FlexibleContexts #-}

module UsingQuickCheck where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

run1 :: IO ()
run1 = quickCheck propHalfIdentity

propHalfIdentity :: Double -> Bool
propHalfIdentity x = x == halfIdentity x

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

run2 :: IO ()
run2 = quickCheck propSort

propSort :: String -> Bool
propSort s = listOrdered (sort s)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

-- For exercie 3 I will use hspec to describe both properties
-- in a single function
run3 :: IO ()
run3 = hspec $ do
  describe "Addition" $ do
    it "is associative" $ do
      property plusAssociative
    it "is commutative" $ do
      property plusCommutative
    it "is associative tested by my new better property" $ do
      property $ isAssociative (+)
    it "is commutative tested by my new better property" $ do
      property $ isCommutative (+)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

run4 :: IO ()
run4 = hspec $ do
  describe "Multiplication" $ do
    it "is associative" $ do
      property $ isAssociative (*)
    it "is commutative" $ do
      property $ isCommutative (*)

isAssociative :: (Int -> Int -> Int) -> Int -> Int -> Int -> Bool
isAssociative f x y z = x `f` (y `f` z) == (x `f` y) `f` z

isCommutative :: (Int -> Int -> Int) -> Int -> Int-> Bool
isCommutative f x y = x `f` y == y `f` x

run5 :: IO ()
run5 = hspec $ do
  describe "Laws for relationships between qout/rem and div/mod" $ do
    it "quot related to rem" $ do
      property $ \x y ->
        y == 0
        || (quot x y) * y + (rem (x :: Int) (y :: Int)) == x
    it "div related to mod" $ do
      property $ \x y ->
        y == 0
        || (div x y) * y + (mod (x :: Int) (y :: Int)) == x

run6 :: IO ()
run6 = hspec $ do
  describe "Power" $ do
    it "is associative" $ do
      property $ isAssociative (^)
    it "is commutative" $ do
      property $ isCommutative (^)

run7 :: IO ()
run7 = quickCheck propDoubleReverse

propDoubleReverse :: String -> Bool
propDoubleReverse s = (reverse . reverse) s == id s

run8 :: IO ()
run8 = do
  quickCheck (propDollarSign reverse :: String -> Bool)
  quickCheck (propComposition reverse sort :: String -> Bool)

propDollarSign :: Eq b => (a -> b) -> a -> Bool
propDollarSign f a = (f $ a) == f a

propComposition :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
propComposition f g x = (f . g) x == f (g x)

run9 :: IO ()
run9 = do
  quickCheck (\x y -> foldr (:) x y == (++) (x :: String) (y :: String) :: Bool)
  quickCheck (\x -> foldr (++) [] x == concat (x :: [[Int]]) :: Bool)

run10 :: IO ()
run10 = quickCheck f10

f10 :: Int -> String -> Bool
f10 n xs = length (take n xs) == n

run11 :: IO ()
run11 = quickCheck f11

f11 :: String -> Bool
f11 x = (read (show x)) == x
