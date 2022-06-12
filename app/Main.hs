module Main where

import Control.Monad (when)
import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Text.Read ()

import Args (getRule, getStart, getLines, getWindow, getMove, ruleToInt,
    startToInt, linesToInt, windowToInt, moveToInt)
import Check (checkNextIsDigit, checkOpts, checkRule)
import Rules (rule30, rule90, rule110)

data Params = Params {
    rule :: Int,
    start :: Int,
    line :: Int,
    window :: Int,
    mov :: Int
} deriving (Show)

firstLast::[Char]->[Char]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

cutStr :: [Char] -> Params -> [Char]
cutStr str params
    | length str - 2 <= window params = str
    | otherwise = cutStr (firstLast str) params

initParams :: Params -> [String] -> Params
initParams params args = Params {rule = ruleToInt(getRule args),
                                start = startToInt(getStart args),
                                line = linesToInt(getLines args),
                                window = windowToInt(getWindow args),
                                mov = moveToInt(getMove args)}

sizeString:: [Char] -> Params -> [Char]
sizeString str params
    | length str > (window params + 1) = take (window params) str ++ "\n"
    |otherwise = str

displayFirstLine :: Params -> [Char]
displayFirstLine params = sizeString (replicate (window params `div` 2
    + mov params) ' ' ++ "*" ++ replicate  (window params -
    ((window params `div` 2) + 1)) ' ' ++ "\n") params

exeRule :: Int -> Char -> Char -> Char -> Char
exeRule 30 a b c = rule30 a b c
exeRule 90 a b c = rule90 a b c
exeRule 110 a b c = rule110 a b c
exeRule _ _ _ _ = ' '

getCharP:: [Char] -> Int -> Char
getCharP str_before index
    | index < 0 = ' '
    | index >= length str_before = ' '
    | otherwise = str_before !! index

algoLine :: [Char] -> [Char] -> Int -> Int -> Params -> [Char]
algoLine str_before res cell index params
    | index > cell = res
    | otherwise = algoLine str_before (res ++ [exeRule (rule params)
    (getCharP str_before (index - 2)) (getCharP str_before (index - 1))
    (getCharP str_before index)]) cell (index + 1) params

displaySpace:: [Char] -> Int -> Int -> Params -> [Char]
displaySpace str_before cell space params = sizeString (replicate
    (space + mov params) ' ' ++ cutStr str_before params ++ replicate  (window params -
    (space + cell)) ' ' ++ "\n") params

lineLoop :: [Char] -> Int -> Int -> Int -> Params -> IO()
lineLoop str_before index cell space params
    | index > (line params + start params) = return ()
    | index > start params = putStr (displaySpace (algoLine str_before [] cell 0 params)
                            cell space params) >>
                     lineLoop (algoLine str_before [] cell 0 params)
                            (index + 1) (cell + 2) (space - 1) params
    | otherwise = lineLoop (algoLine str_before [] cell 0 params)
                            (index + 1) (cell + 2) (space - 1) params

lineLoopInf :: [Char] -> Int -> Int -> Int -> Params -> IO()
lineLoopInf str_before index cell space params
    | index < -1 = return ()
    | otherwise = putStr (displaySpace (algoLine str_before [] cell 0 params)
                        cell space params) >>
                     lineLoopInf (algoLine str_before [] cell 0 params) index
                        (cell + 2) (space - 1) params

main :: IO ()
main = do
    args <- getArgs
    when (checkOpts args || checkRule args || windowToInt(getWindow args) < 0 ||
        startToInt(getStart args) < 0|| linesToInt(getLines args) < -1) $
        putStrLn "Wrong Arguments!" >> exitWith (ExitFailure 84)
    let opts = Params {rule = 0, start = 0, line = 0, window = 0, mov = 0}
    let params = initParams opts args
    if line params == -1
        then putStr (displayFirstLine params) >>
        lineLoopInf ['*'] 2 2 (window params `div` 2 - 1) params
    else if line params == 0
        then return()
    else if start params > 0
        then lineLoop ['*'] 2 2 (window params `div` 2 - 1) params
    else do
        putStr (displayFirstLine params)
        lineLoop ['*'] 2 2 (window params `div` 2 - 1) params
    return()