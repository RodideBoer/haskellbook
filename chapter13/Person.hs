module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == ""            = Left NameEmpty
    | not (age > 0)         = Left AgeTooLow
    | otherwise             = Left $ PersonInvalidUnknown $
                                "Name was: " ++ show name ++
                                "Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Give me a name: "
    name <- getLine
    putStr "Give me an age: "
    ageString <- getLine
    let age = read ageString :: Integer
    let person = mkPerson name age
    case person of
        Right r -> putStrLn $ "Yay! Successfully got a person: " ++ show r
        Left l  -> putStrLn $ "An error occurred: " ++ show l
