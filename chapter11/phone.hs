module Phone where

import Data.Char
import Data.List
import Data.Maybe

-- valid digits                        = "1234567890*#"
type Digit                             = Char
-- Valid letters                       = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "* +,.#"
type Letters                           = String
-- Valid presses: 1 and up
type Presses                           = Int

data Button                            = Button Digit Letters
data DaPhone                           = DaPhone [Button]

myPhone :: DaPhone
myPhone                                = DaPhone
        [ Button '1' "1"
        , Button '2' "ABC2"
        , Button '3' "DEF3"
        , Button '4' "GHI4"
        , Button '5' "JKL5"
        , Button '6' "MNO6"
        , Button '7' "PQRS7"
        , Button '8' "TUV8"
        , Button '9' "WXYZ9"
        , Button '*' "^*"
        , Button '0' " +"
        , Button '#' ".,#"
        ]

convo :: [String]
convo                                  =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]

buttonPresses :: Button -> Char -> Maybe (Digit, Presses)
buttonPresses (Button digit letters) c = case elemIndex (toUpper c) letters of
    Just index -> Just (digit, index + 1)
    Nothing -> Nothing

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone@(DaPhone buttons) c
    | isUpper c = reverseTaps phone '^' ++ findButtonPressesFor
    | otherwise = findButtonPressesFor
        where findButtonPressesFor = foldr (\a b -> maybeToList (buttonPresses a c) ++b) [] buttons

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = foldr (\a b -> (++) (reverseTaps phone a) b) []

convoTapping :: [[(Digit, Presses)]]
convoTapping = map (cellPhonesDead myPhone) convo

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr addPresses 0
    where addPresses (_,p) b = p + b
-- with pattern matching in lambda, but making it less clear
-- fingerTaps                          = foldr (\(_,p) b -> b + p) 0
fingerTaps' :: [(Digit, Presses)] -> Presses
fingerTaps' = sum . map snd

tapsForAllMessages :: [Presses]
tapsForAllMessages = map (fingerTaps . cellPhonesDead myPhone) convo

mostPopularLetter :: String -> Char
mostPopularLetter [] = '-' -- really should be an exception
mostPopularLetter xs = fst $ last $ sortOn snd $ letterCounts xs

letterCounts :: String -> [(Char, Int)]
letterCounts = countThem . sort
    where countThem []      = []
          countThem s@(x:_) = if isAlphaNum x
            then (x, length (takeWhile (==x) s)) : countThem (dropWhile (==x) s)
            else countThem (dropWhile (==x) s)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

letterTapCost :: Char -> Presses
letterTapCost = fingerTaps . reverseTaps myPhone

mostPopularSomething :: Ord a => [a] -> a
mostPopularSomething [] = undefined -- really should be an exception
mostPopularSomething xs = fst $ last $ sortOn snd $ somethingCounts xs

somethingCounts :: Ord a => [a] -> [(a, Int)]
somethingCounts = countThem . sort
    where countThem []      = []
          countThem s@(x:_) = (x, length (takeWhile (==x) s)) : countThem (dropWhile (==x) s)

toWords :: String -> [String]
toWords [] = []
toWords xs = takeWhile (/=' ') xs : toWords (dropWhile (==' ') (dropWhile (/=' ') xs))

coolestWord :: [String] -> String
coolestWord = mostPopularSomething . toWords . concat
