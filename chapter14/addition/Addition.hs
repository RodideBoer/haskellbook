module Addition where

import Test.Hspec
import Test.QuickCheck

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1 :: Integer) + 1) > 1 `shouldBe` True
    it "2 + 2 should equal to 4" $ do
      (2 :: Integer) + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

    it "15 divided by 3 is 5" $ do
      (dividedBy 15 3 :: (Integer, Integer)) `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      (dividedBy 22 5 :: (Integer, Integer)) `shouldBe` (4, 2)
    it "3 times 4 is 12" $ do
      (multi 3 4 :: Integer) `shouldBe` 12
    it "0 times 5 is 0" $ do
      (multi 0 5 :: Integer) `shouldBe` 0
    it "2 times 0 is 0" $ do
      (multi 2 0 :: Integer) `shouldBe` 0
    it "3 times -2 is -6" $ do
      (multi 3 (-2) :: Integer) `shouldBe` (-6)
    it "-3 times 5 is -15" $ do
      (multi (-3) 5 :: Integer) `shouldBe` (-15)
    it "-3 times -4 is 12" $ do
      (multi (-3) (-4) :: Integer) `shouldBe` 12

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)

multi :: (Eq a, Num a, Ord a) => a -> a -> a
multi x y
  | y == 0    = 0
  | y < 0     = -multi x (-y)
  | otherwise = x + multi x (y - 1)

genBool :: Gen Bool
genBool = choose (False, True)
genBool' :: Gen Bool
genBool' = elements [False, True]
genBool'' :: Gen Bool
genBool'' = elements [False, False, False, False, False, False, False, True]
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]
genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck actually does
-- so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]
-- frequency :: [(Int, Gen a)] -> Gen a
