module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
                (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
    , DbNumber 42
    , DbNumber 1
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate []            = []
filterDbDate (DbDate x:xs) = x : filterDbDate xs
filterDbDate (_:xs)        = filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber []              = []
filterDbNumber (DbNumber x:xs) = x : filterDbNumber xs
filterDbNumber (_:xs)          = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr
                    (\a b -> if a > b then a else b)
                    (UTCTime
                        (fromGregorian 0 0 0)
                        (secondsToDiffTime 0))
                    $ filterDbDate xs

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0 $ filterDbNumber xs

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sum allNumbers) / fromIntegral (length allNumbers)
    where allNumbers = filterDbNumber xs
