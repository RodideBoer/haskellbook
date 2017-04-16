module Cipher where

import Data.Char
import Test.QuickCheck

runCipherTests :: IO ()
runCipherTests = do
  quickCheck propCaesarIdentity
  quickCheck propVigenereIdentity

propCaesarIdentity :: Int -> String -> Bool
propCaesarIdentity x s = s == (unCaesar' x . caesar' x) s

propVigenereIdentity :: String -> String -> Bool
propVigenereIdentity k s = s == (unVigenere' k . vigenere' k) s

{-
  Cipher functions copied from Chapter 13 with some modifications to make the identities work
  Characters with accents are seen as Alpha, Upper etc. but do not work with my base 26 shifting algorithm
  First I wanted to create my own isUpper functions, but I discovered the Ascii versions and they work
-}
caesar' :: Int -> String -> String
caesar' a s = map shiftChar s
    where shiftChar c
            | isAsciiUpper c = shiftFrom 'A'
            | isAsciiLower c = shiftFrom 'a'
            | otherwise      = c
                where shiftFrom base = chr (mod (ord c + a - ord base) 26 + ord base)


unCaesar' :: Int -> String -> String
unCaesar' a = caesar' (-a)

vigenere' :: String -> String -> String
vigenere' _ []  = []
vigenere' key s = go key s 0
    where go _ [] _     = []
          go [] s' _    = s'
          go k (x:xs) i = shiftChar x (charAt k i) : go k xs (i + (if isAsciiLetter x then 1 else 0))
          isAsciiLetter c = isAsciiUpper c || isAsciiLower c
          charAt k i = k !! mod i (length k)
          baseOrd c
            | isAsciiUpper c = ord 'A'
            | isAsciiLower c = ord 'a'
            | otherwise      = 0
          shiftChar c k
            | isAsciiLetter c && isAsciiLetter k
              = chr (mod (ord c - baseOrd c + ord k - baseOrd k) 26 + baseOrd c)
            | otherwise
              = c

unVigenere' :: String -> String -> String
unVigenere' s = vigenere' (map negChar s)
    where negChar c
            | isAsciiUpper c = chr (mod (ord 'Z' - ord c + 1) 26 + ord 'A')
            | isAsciiLower c = chr (mod (ord 'z' - ord c + 1) 26 + ord 'a')
            | otherwise      = c
