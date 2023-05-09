module Lib
    ( someFunc
    ) where

import Data.Aeson
import Data.Maybe
import Control.Applicative ()
import qualified Data.ByteString.Lazy as B
import MachineType (
    Machine (Machine, line, pointer, currentState),
    Rule (stateFrom, stateTo, currentLetter, move, newLetter),
    Move (Left, Right, Same),
    getItem, putItem)


offset :: Move -> Int
offset MachineType.Left = -1
offset MachineType.Right = 1
offset MachineType.Same = 0

findRule :: Machine -> Maybe Rule
findRule (Machine r l c p) | null r = Nothing
           | (stateFrom (head r) == c) && (getItem l p == currentLetter (head r)) = Just (head r)
           | otherwise = findRule (Machine (tail r) l c p)

makeStep :: Machine -> Maybe Machine
makeStep (Machine r l c p) | isNothing rule = Nothing
                           | p + offset (move ( fromJust rule )) < 0 = Nothing
                           | isNothing (newLetter (fromJust rule)) = Just (Machine r l (stateTo (fromJust rule)) (offset (move (fromJust rule)) + p))
                           | otherwise = Just (Machine r (putItem l p (fromJust (newLetter (fromJust rule)))) (stateTo (fromJust rule)) (offset (move (fromJust rule)) + p))
                           where rule = findRule (Machine r l c p)




showAll :: Maybe Machine -> IO ()
showAll Nothing = print "Terminal state reached!"
showAll (Just m) = do
    inp <- getLine
    case inp of
        "stop" -> print "Process stopped!"
        "next" -> do
            print (currentState m)
            print (line m)
            print (pointer m)
            print (findRule m)
            showAll (makeStep m)
        _ -> do
            print "failed to parse input!"
            showAll (Just m)

someFunc :: IO ()
someFunc = do
    input <- B.readFile "test.json"
    let mm = decode input :: Maybe MachineType.Machine
    case mm of
        Nothing -> print "error parsing Machine json"
        Just m -> showAll (Just m)