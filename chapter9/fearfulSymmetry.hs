module FearfulSymmetry where

myWords :: String -> [String]
myWords s
    | s == [] = []
    | otherwise = takeWhile notSpace s : myWords nextWord
        where
            nextWord = dropWhile isSpace . dropWhile notSpace $ s
            notSpace = (/=' ')
            isSpace = (==' ')
