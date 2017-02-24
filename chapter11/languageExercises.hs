module LanguageExercises where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph xs = capitalizeWord (takeWhile (/='.') xs) ++ takeWhile (\a -> a == '.' || a == ' ') (dropWhile (/='.') xs) ++
                         capitalizeParagraph (dropWhile (\a -> a == '.' || a == ' ') (dropWhile (/='.') xs))
