module StringProcessing where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

-- searched for intercalate to stop me from having to hassle with adding the spaces back
-- but the linter in ide-haskell (atom package) suggested that unwords was an alternative for (intercalate " ")
replaceThe :: String -> String
replaceThe s = unwords $ map (the2a . notThe) (words s)
    where the2a Nothing  = "a"
          the2a (Just a) = a

vowels :: String
vowels = "aeiou"

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = go 0 $ map notThe (words s)
    where go acc []         = acc
          go acc [_]        = acc
          go acc (x1:(xs@(x2:_))) = go (if isTheBeforeVowel x1 x2 then acc + 1 else acc) xs
          isTheBeforeVowel Nothing (Just b) = elem (head b) vowels
          isTheBeforeVowel _ _              = False
