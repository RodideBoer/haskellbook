module Tests where

import Main
import Test.Hspec
import Data.List

-- There are probably better ways to test, especially for handleGuess, but this is what I could think of
-- Maybe by creating generators for every part of Puzzle and then using QuickCheck

testPuzzle :: Puzzle
testPuzzle = freshPuzzle "test"

tGuessedPuzzle :: Puzzle
tGuessedPuzzle = fillInCharacter testPuzzle 't'

puzzleHasWord :: String -> Puzzle -> Bool
puzzleHasWord s (Puzzle w _ _) = w == s

puzzleFilledIn :: [Maybe Char] -> Puzzle -> Bool
puzzleFilledIn xs (Puzzle _ filledIn _) = filledIn == xs

puzzleHasGuessed :: [Char] -> Puzzle -> Bool
puzzleHasGuessed xs (Puzzle _ _ guessed) = sort guessed == sort xs

runTests :: IO ()
runTests = hspec $ do
  describe "fillInCharacter" $ do
    it "should leave the word alone" $ do
      fillInCharacter testPuzzle 'a' `shouldSatisfy` puzzleHasWord "test"
    it "should fill in a single character" $ do
      fillInCharacter testPuzzle 'e' `shouldSatisfy` puzzleFilledIn [Nothing, Just 'e', Nothing, Nothing]
    it "should fill in a multiple characters" $ do
      fillInCharacter testPuzzle 't' `shouldSatisfy` puzzleFilledIn [Just 't', Nothing, Nothing, Just 't']
    it "should add right character to guessed characters" $ do
      fillInCharacter testPuzzle 't' `shouldSatisfy` puzzleHasGuessed "t"
    it "should add wrong character to guessed characters" $ do
      fillInCharacter testPuzzle 'a' `shouldSatisfy` puzzleHasGuessed "a"
    it "should add another character to guessed characters" $ do
      fillInCharacter (fillInCharacter (fillInCharacter testPuzzle 't') 'a') 'x' `shouldSatisfy` puzzleHasGuessed "atx"
  describe "handleGuess" $ do
    describe "when guessing the same character" $ do
      it "should return the same puzzle" $ do
        doubleT <- handleGuess tGuessedPuzzle 't'
        doubleT `shouldSatisfy` puzzleHasWord "test"
        doubleT `shouldSatisfy` puzzleFilledIn [Just 't', Nothing, Nothing, Just 't']
        doubleT `shouldSatisfy` puzzleHasGuessed "t"
    describe "when guessing a new correct character" $ do
      it "should return the puzzle with the character filled in" $ do
        newPuzzle <- handleGuess tGuessedPuzzle 'e'
        newPuzzle `shouldSatisfy` puzzleFilledIn [Just 't', Just 'e', Nothing, Just 't']
      it "should return the puzzle with the character guessed" $ do
        newPuzzle <- handleGuess tGuessedPuzzle 'e'
        newPuzzle `shouldSatisfy` puzzleHasGuessed "et"
    describe "when guessing a new correct character" $ do
      it "should return the puzzle without the character filled in" $ do
        newPuzzle <- handleGuess tGuessedPuzzle 'm'
        newPuzzle `shouldSatisfy` puzzleFilledIn [Just 't', Nothing, Nothing, Just 't']
      it "should return the puzzle with the character guessed" $ do
        newPuzzle <- handleGuess tGuessedPuzzle 'm'
        newPuzzle `shouldSatisfy` puzzleHasGuessed "mt"
