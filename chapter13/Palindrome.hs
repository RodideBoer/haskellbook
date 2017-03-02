module Palindrome where

import           Control.Monad
import           Data.Char
import           System.Exit   (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
    line <- getLine
    let bareText = map toLower $ stripBare line
    case (bareText == reverse bareText) of
        True -> putStrLn "It's a palindrome!"
        False -> do
                  putStrLn "Nope!"
                  exitSuccess

stripBare :: String -> String
stripBare = filter isAlpha
