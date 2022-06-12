module Rules
    (
        rule30,
        rule90,
        rule110
    ) where

import Text.Read ()
import Control.Exception ()

import Args (getRule, getStart, getLines, getWindow, getMove, ruleToInt,
    startToInt, linesToInt, windowToInt, moveToInt)
import Check (checkNextIsDigit, checkOpts, checkRule)

rule30 :: Char -> Char -> Char -> Char
rule30 '*' ' ' ' ' = '*'
rule30 ' ' '*' '*' = '*'
rule30 ' ' '*' ' ' = '*'
rule30 ' ' ' ' '*' = '*'
rule30 _ _ _= ' '

rule90 :: Char -> Char -> Char -> Char
rule90 '*' '*' ' ' = '*'
rule90 '*' ' ' ' ' = '*'
rule90 ' ' '*' '*' = '*'
rule90 ' ' ' ' '*' = '*'
rule90 _ _ _= ' '

rule110 :: Char -> Char -> Char -> Char
rule110 '*' '*' ' ' = '*'
rule110 '*' ' ' '*' = '*'
rule110 ' ' '*' '*' = '*'
rule110 ' ' '*' ' ' = '*'
rule110 ' ' ' ' '*' = '*'
rule110 _ _ _= ' '