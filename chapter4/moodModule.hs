module MoodModule where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

testInt :: Int -> Int
testInt 2 = 100
testInt _ = 2

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x > 0
          then x
          else x * (-1)

f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f x y = ((snd x, snd y), (fst x, fst y))

func :: Foldable t => t a -> Int
func xs = w `xx` 1
    where w = length xs
          xx = (+)
