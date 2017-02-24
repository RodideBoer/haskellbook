module Test where

newtype Nada = Nada Double deriving (Eq, Show)

instance Num Nada where
    (+) (Nada a) (Nada b) = Nada ((+) a b)
    (*) (Nada a) (Nada b) = Nada ((*) a b)
    abs (Nada a) = Nada (abs a)
    signum (Nada a) = Nada (signum a)
    fromInteger i = Nada (fromInteger i)
    negate (Nada a) = Nada (negate a)

instance Fractional Nada where
    (Nada x) / (Nada y) = Nada (x / y)
    recip (Nada n) = Nada (recip n)
    fromRational r = Nada (fromRational r)
