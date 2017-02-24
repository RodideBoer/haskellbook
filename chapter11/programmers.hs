module Programmers where

data OperatingSystem =
        GnuPlusLinux
      | OpenBSDPlusNevermindJustBSDStill
      | Mac
      | Windows
        deriving (Eq, Show)

data ProgrammingLanguage =
        Haskell
      | Agda
      | Idris
      | PureScript
        deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgrammingLanguage }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = o , lang = l }
                 | o <- allOperatingSystems, l <- allLanguages
                 ]

assertAllProgrammers :: Bool
assertAllProgrammers =
    (length allOperatingSystems * length allLanguages) == length allProgrammers
