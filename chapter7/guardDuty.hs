module GuardDuty where

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.7  = 'C'
    | y >= 0.59 = 'D'
    | otherwise = 'F'
    where y = x / 100

pal xs
    | xs == reverse xs = True
    | otherwise        = False

numbers x
    | x < 0  = -1
    | x == 0 = 0
    | x > 0  = 1
