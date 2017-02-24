module TypeKwonDo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (aToB a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB x a = fromInteger x + (aToB a)
