module Cipher where

import Data.Char

caesar :: IO String
caesar = do
    putStr "Text to cipher: "
    text <- getLine
    putStr "Shift by: "
    shift <- readLn :: IO Int
    return $ caesar' shift text

unCaesar :: IO String
unCaesar = do
    putStr "Text to decipher: "
    text <- getLine
    putStr "Was shifted by: "
    shift <- readLn :: IO Int
    return $ caesar' (-shift) text

vigenere :: IO String
vigenere = do
    putStr "Text to cipher: "
    text <- getLine
    putStr "Key to cipher: "
    key <- getLine
    return $ vigenere' key text

unVigenere :: IO String
unVigenere = do
    putStr "Text to decipher: "
    text <- getLine
    putStr "Was ciphered with key: "
    key <- getLine
    return $ unVigenere' key text

caesar' :: Int -> String -> String
caesar' a s = map shiftChar s
    where shiftChar c
            | isAlpha c && isUpper c = shiftFrom 'A'
            | isAlpha c && isLower c = shiftFrom 'a'
            | otherwise              = c
                where shiftFrom base = chr (mod (ord c + a - ord base) 26 + ord base)


unCaesar' :: Int -> String -> String
unCaesar' a = caesar' (-a)

vigenere' :: String -> String -> String
vigenere' _ []  = []
vigenere' key s = go key s 0
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

unVigenere' :: String -> String -> String
unVigenere' s = vigenere' (map negChar s)
    where negChar c
            | isAlpha c && isUpper c = chr (mod (ord 'Z' - ord c + 1) 26 + ord 'A')
            | isAlpha c && isLower c = chr (mod (ord 'z' - ord c + 1) 26 + ord 'a')
            | otherwise              = c
