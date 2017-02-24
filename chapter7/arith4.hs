module Arith4 where

--id :: a -> a
--id x = x

roundTrip :: (Show a, Read b) => a -> b
roundTrip x = read . show $ x

main = do
    print ((roundTrip 4) :: Integer)
    print (id 4)
