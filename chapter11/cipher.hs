module Cipher where

import Data.Char

vigenere :: String -> String -> String
vigenere _ []  = []
vigenere key s = go key s 0
    where go _ [] _     = []
          go k (x:xs) i = shiftChar x (charAt k i) : go k xs (i + (if isAlpha x then 1 else 0))
          charAt k i = k !! mod i (length k)
          baseOrd c
            | isAlpha c && isUpper c = ord 'A'
            | isAlpha c && isLower c = ord 'a'
            | otherwise              = 0
          shiftChar c k
            | isAlpha c = chr (mod (ord c - baseOrd c + ord k - baseOrd k) 26 + baseOrd c)
            | otherwise = c

unVigenere :: String -> String -> String
unVigenere s = vigenere (map negChar s)
    where negChar c
            | isAlpha c && isUpper c = chr (mod (ord 'Z' - ord c + 1) 26 + ord 'A')
            | isAlpha c && isLower c = chr (mod (ord 'z' - ord c + 1) 26 + ord 'a')
            | otherwise              = c
