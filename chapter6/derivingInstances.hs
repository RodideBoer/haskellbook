module DerivingInstances where

data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Ord, Show)

data Date =
    Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _   _   = False

instance Eq Date where
    (==) (Date dayOfWeek dayOfMOnth)
         (Date dayOfWeek' dayOfMOnth') =
             dayOfWeek == dayOfWeek' && dayOfMOnth == dayOfMOnth'

data Identity a =
    Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'
