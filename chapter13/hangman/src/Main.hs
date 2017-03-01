module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse, sort)
import           Data.Maybe    (fromMaybe, isJust)
import           System.Exit   (exitSuccess)
import           System.IO
import           System.Random (randomRIO)

-- Words

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in  l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, length wl)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- Puzzle
data Puzzle = Puzzle String [Maybe Char] [Char]

maxWrongGuesses :: Int
maxWrongGuesses = 10

instance Show Puzzle where
    show puzzle@(Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guesses so far: " ++ guessed
        ++ " (Wrong: " ++ show (wrongNumberOfGuesses puzzle) ++ "/10)"

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (map (const Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = fromMaybe '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (sort (c:s))
    where zipper guessed wordChar soFarChar = if wordChar == guessed
            then Just wordChar
            else soFarChar
          newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case ( charInWord puzzle guess
         , alreadyGuessed puzzle guess ) of
        (_, True) -> do
            putStrLn "You already guessed that character, \
                \pick something else!"
            return puzzle
        (False, _) -> do
            putStrLn "Wrong! That character we do not have."
            return $ fillInCharacter puzzle guess
        (True, _) -> do
            putStrLn "We have that character, filling it in right now."
            return $ fillInCharacter puzzle guess

wrongNumberOfGuesses :: Puzzle -> Int
wrongNumberOfGuesses (Puzzle word _ guessed) =
    length (filter (not . flip elem word) guessed)

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle word _ _) =
    if wrongNumberOfGuesses puzzle >= maxWrongGuesses
    then do
        putStrLn "You lose!"
        putStrLn $ "The word was: " ++ word
        exitSuccess
    else
        return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) = if all isJust filledInSoFar
    then do
        putStrLn "Well done, you win!"
        exitSuccess
    else
        return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Please enter a single character only..."

-- Main
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
