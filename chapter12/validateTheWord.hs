module ValidateTheWord where

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = if countBy isVowel s > countBy isConsonant s
    then Nothing
    else Just (Word' s)

countBy :: (Char -> Bool) -> String -> Integer
countBy f s = fromIntegral $ length $ filter f s

isVowel :: Char -> Bool
isVowel c = elem c vowels

isConsonant :: Char -> Bool
isConsonant = not . isVowel
