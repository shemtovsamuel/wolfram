module Check
    (
        checkNextIsDigit,
        checkOpts,
        checkRule
    ) where

import Text.Read ()
import Control.Exception ()

import Args (getRule, getStart, getLines, getWindow, getMove, ruleToInt,
    startToInt, linesToInt, windowToInt, moveToInt)

myIsDigit :: Char -> Bool
myIsDigit a = a <= '9' && a >= '0' || a == '-'

myStrIsDigit :: String -> Bool
myStrIsDigit = all myIsDigit

checkNextIsDigit :: String -> Bool
checkNextIsDigit [] = False
checkNextIsDigit str
    | myStrIsDigit str = True
    | otherwise = False

checkOpts :: [String] -> Bool
checkOpts [] = False
checkOpts list | odd (length list) = True
checkOpts (var:next:xs)
    | var `elem` words "--rule --start --lines --window --move"
        && checkNextIsDigit next = checkOpts xs
    | otherwise = True

checkRule :: [String] -> Bool
checkRule [] = True
checkRule args | ruleToInt(getRule args) == 30 || ruleToInt(getRule args) == 90
    || ruleToInt(getRule args) == 110 = False
    | otherwise = True