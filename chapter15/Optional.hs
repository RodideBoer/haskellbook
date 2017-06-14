module Optional where

import Data.Monoid
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend x Nada    = x
  mappend Nada y    = y
  mappend (Only x) (Only y) = Only (mappend x y)

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = do
  a <- arbitrary
  oneof [ return (Only a)
        , return Nada ]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = optionalGen

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) y = y
  mappend x y = x

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  return (First' a)

firstGenString :: Gen (First' String)
firstGenString = firstGen

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
