module Exercises where

stops :: String
stops = "pbtdkg"
vowels :: String
vowels = "aeiou"

svs :: [(Char, Char, Char)]
svs = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

svsP :: [(Char, Char, Char)]
svsP = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns :: [String]
nouns =
    [ "school"
    , "laptop"
    , "book"
    , "table"
    , "door"
    ]
verbs :: [String]
verbs =
    [ "walks"
    , "eats"
    , "picks up"
    , "drops"
    , "writes"
    ]

nvn :: [(String, String, String)]
nvn = [(xs, ys, zs) | xs <- nouns, ys <- verbs, zs <- nouns]

seekritFunc :: String -> Int
-- average length (rounded down) of the words in a sentence
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))

avgWordLength :: (Fractional a) => String -> a
avgWordLength x =
    fromIntegral (sum (map length (words x)))
        / fromIntegral (length (words x))
