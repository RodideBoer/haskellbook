module EqInstances where

data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a1) (TisAn a2) =
        a1 == a2

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') =
        x == x' && y == y'

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a')     = a == a'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _ _                            = False

data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne b) = a == b
    (==) (ThatOne a) (ThatOne b) = a == b
    (==) _ _                     = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a')     = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _                      = False
