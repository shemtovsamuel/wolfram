module Args
    (
        getRule,
        getStart,
        getLines,
        getWindow,
        getMove,
        ruleToInt,
        startToInt,
        linesToInt,
        windowToInt,
        moveToInt
    ) where

import Text.Read ()
import Control.Exception ()


getRule :: [String] -> Maybe String
getRule [] = Nothing
getRule ("--rule":next:_) = Just next
getRule (_:xs) = getRule xs

getStart :: [String] -> Maybe String
getStart [] = Nothing
getStart ("--start":next:_) = Just next
getStart (_:xs) = getStart xs

getLines :: [String] -> Maybe String
getLines [] = Nothing
getLines ("--lines":next:_) = Just next
getLines (_:xs) = getLines xs

getWindow :: [String] -> Maybe String
getWindow [] = Nothing
getWindow ("--window":next:_) = Just next
getWindow (_:xs) = getWindow xs

getMove :: [String] -> Maybe String
getMove [] = Nothing
getMove ("--move":next:_) = Just next
getMove (_:xs) = getMove xs

ruleToInt :: Maybe String -> Int
ruleToInt (Just str) = read str :: Int
ruleToInt Nothing = -84

startToInt :: Maybe String -> Int
startToInt (Just str) = read str :: Int
startToInt Nothing = 0

linesToInt :: Maybe String -> Int
linesToInt (Just str) = read str :: Int
linesToInt Nothing = -1

windowToInt :: Maybe String -> Int
windowToInt (Just str) = read str :: Int
windowToInt Nothing = 80

moveToInt :: Maybe String -> Int
moveToInt (Just str) = read str :: Int
moveToInt Nothing = 0