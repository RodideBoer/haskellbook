module FoolGen where

import Test.QuickCheck

foolGen :: IO ()
foolGen = do
  putStrLn "** genFool **"
  sample genFool
  putStrLn "** genFoolMoreFulse **"
  sample genFoolMoreFulse

data Fool =
    Fulse
  | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFoolMoreFulse :: Gen Fool
genFoolMoreFulse = frequency
  [ (2, return Fulse)
  , (1, return Frue)
  ]
