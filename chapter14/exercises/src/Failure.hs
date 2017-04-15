module Failure where

import Test.QuickCheck

failure :: IO ()
failure = quickCheck propSquare

propSquare :: Double-> Bool
propSquare x = x == squareIdentity x

square x = x * x
-- why does this property not hold? Examine the type of sqrt.
squareIdentity = square . sqrt
