module DataTypes (
    Machine
) where

import GHC.Generics

data Machine = Machine {
    alphabet :: [Char],
    rules :: [Rule],
    line :: [Char]
} deriving (Show)

data Rule = Rule {
    stateFrom :: String,
    currentLetter :: Char,
    stateTo :: String,
    newLetter :: Char
    --move :: Move
} deriving (Show)

data Move = Left | Right | Same deriving (Eq, Show)