module Cipher where

import Data.Char

--http://rosettacode.org/wiki/Caesar_cipher#Haskell

caesarV1 :: Int -> String -> String
caesarV1 _ []     = []
caesarV1 a (x:xs) = shift a x : caesarV1 a xs
    where shift a' x'
            | isUpper x' = shiftFrom 'A' a' x'
            | otherwise  = shiftFrom 'a' a' x'
                where shiftFrom c a'' x'' = chr (mod (ord x'' + a'' - ord c) 26 + ord c)

caesarV2 :: Int -> String -> String
caesarV2 _ []     = []
caesarV2 a (x:xs) = shift : caesarV2 a xs
    where shift
            | isUpper x  = shiftFrom 'A'
            | otherwise  = shiftFrom 'a'
                where shiftFrom c = chr (mod (ord x + a - ord c) 26 + ord c)

caesar :: Int -> String -> String
caesar a s = map shiftChar s
    where shiftChar c
            | isAlpha c && isUpper c = shiftFrom 'A'
            | isAlpha c && isLower c = shiftFrom 'a'
            | otherwise              = c
                where shiftFrom base = chr (mod (ord c + a - ord base) 26 + ord base)


unCaesar :: Int -> String -> String
unCaesar a = caesar (-a)
