{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/07-folds-monoids.pdf-}


{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char as C



newtype Score = Score Int deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)


score :: Char -> Score
score c = case C.toUpper c of
    'A' -> Score 1
    'B' -> Score 2
    'C' -> Score 3
    'D' -> Score 2
    'E' -> Score 1
    'F' -> Score 4
    'G' -> Score 2
    'H' -> Score 4
    'I' -> Score 1
    'J' -> Score 8
    'K' -> Score 5
    'L' -> Score 1
    'M' -> Score 3
    'N' -> Score 1
    'O' -> Score 1
    'P' -> Score 3
    'Q' -> Score 10
    'R' -> Score 1
    'S' -> Score 1
    'T' -> Score 1
    'U' -> Score 1
    'V' -> Score 4
    'W' -> Score 4
    'X' -> Score 8
    'Y' -> Score 4
    'Z' -> Score 10
    otherwise -> Score 0


scoreString :: String -> Score
scoreString = mconcat . map score



