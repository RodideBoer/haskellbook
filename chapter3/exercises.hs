module Exercise where

fa :: String -> String
fa x = x ++ "!"

fb :: String -> String
fb x = take 1 (drop 4 x)

fc :: String -> String
fc x = drop 9 x

third :: String -> Char
third x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs x = (drop 9 x) ++ (take 4 (drop 5 x)) ++ (take 5 x)
