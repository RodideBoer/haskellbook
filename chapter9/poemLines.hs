module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
            ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this

breakString :: Char -> String -> [String]
breakString c s
    | s == [] = []
    | otherwise = takeWhile notEqualsChar s : breakString c nextLine
        where
            nextLine = (dropWhile equalsChar . dropWhile notEqualsChar $ s)
            equalsChar = (== c)
            notEqualsChar = (/= c)


myLines :: String -> [String]
myLines s = breakString '\n' s
    -- | s == [] = []
    -- | otherwise = takeWhile (/='\n') s : myLines nextLine
    --     where nextLine = (dropWhile (=='\n') . dropWhile (/='\n') $ s)

myWords :: String -> [String]
myWords s = breakString ' ' s

-- What we want 'myLines sentences' to equal
shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?" ]

-- The main function here is a small test -- to ensure you've written your function -- correctly.
main :: IO ()
main =
    print $ "Are they equal? "
          ++ show (myLines sentences == shouldEqual)
          ++ "\n"
          ++ "Are they equal? "
          ++ show (myWords "This is the string that should be split" == ["This", "is", "the", "string", "that", "should", "be", "split"])
