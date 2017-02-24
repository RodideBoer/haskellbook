module OnlyNatural where

data Nat =
      Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = natToInteger n + 1

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0     = Nothing
    | otherwise = Just (go x)
        where go n
                | n == 0    = Zero
                | otherwise = Succ (go (pred n))
