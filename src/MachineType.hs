{-# LANGUAGE OverloadedStrings #-}
module MachineType (
    Machine (..),
    Rule (..),
    Move (..),
    getItem,
    putItem
) where


import Data.Aeson
import Control.Monad
import Data.Text.Conversions

data Machine = Machine {
    rules :: [Rule],
    line :: Line,
    currentState :: [Char],
    pointer :: Int
} deriving (Show)
instance FromJSON Machine where
    parseJSON (Object v) = Machine
        <$> v.: "rules"
        <*> v.: "line"
        <*> v.: "currentState"
        <*> v.: "pointer"
    parseJSON _ = mzero



data Rule = Rule {
    stateFrom :: String,
    currentLetter :: Char,
    stateTo :: String,
    newLetter :: Maybe Char,
    move :: Move
} deriving (Show)
instance FromJSON Rule where
    parseJSON (Object v) = Rule
        <$> v.: "stateFrom"
        <*> v.: "currentLetter"
        <*> v.: "stateTo"
        <*> v.:? "newLetter"
        <*> v.: "move"
    parseJSON _ = mzero



data Move = Left | Right | Same deriving (Eq, Show)
instance FromJSON Move where
    parseJSON (String "Left") = return MachineType.Left
    parseJSON (String "Right") = return MachineType.Right
    parseJSON (String "Same") = return MachineType.Same
    parseJSON _ = mzero


data Line = Line {
    line_l :: [Char],
    line_r :: [Char]
} deriving (Show, Eq)
instance FromJSON Line where
    parseJSON (String s) = return Line {line_l = "", line_r = fromText s}
    parseJSON _ = mzero

getItem :: Line -> Int -> Char
getItem (Line l r) index | index >= length r || -index > length l = '_'
                         | index >= 0 = r !! index
                         | otherwise = r !! (-index - 1)

putItem :: Line -> Int -> Char -> Line
putItem (Line l r) index c | index >= 0 = Line l (insertIntoEndless r index c)
                           | otherwise = Line (insertIntoEndless l (-index - 1) c) r

insertIntoEndless :: String -> Int-> Char -> String
insertIntoEndless [] 0 c = [c]
insertIntoEndless [] index c = '_' : insertIntoEndless [] (index - 1) c
insertIntoEndless (_ : hs) 0 c = c : hs
insertIntoEndless (h : hs) index c = h : insertIntoEndless hs (index - 1) c