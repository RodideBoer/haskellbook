module Bull where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
