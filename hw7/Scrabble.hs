{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/07-folds-monoids.pdf-}


{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char as C



newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)


score :: Char -> Score
score c
    | upperC == 'A'     = Score 1
    | upperC == 'B'     = Score 2
    | upperC == 'C'     = Score 3
    | upperC == 'D'     = Score 2
    | upperC == 'E'     = Score 1
    | upperC == 'F'     = Score 4
    | upperC == 'G'     = Score 2
    | upperC == 'H'     = Score 4
    | upperC == 'I'     = Score 1
    | upperC == 'J'     = Score 8
    | upperC == 'K'     = Score 5
    | upperC == 'L'     = Score 1
    | upperC == 'M'     = Score 3
    | upperC == 'N'     = Score 1
    | upperC == 'O'     = Score 1
    | upperC == 'P'     = Score 3
    | upperC == 'Q'     = Score 10
    | upperC == 'R'     = Score 1
    | upperC == 'S'     = Score 1
    | upperC == 'T'     = Score 1
    | upperC == 'U'     = Score 1
    | upperC == 'V'     = Score 4
    | upperC == 'W'     = Score 4
    | upperC == 'X'     = Score 8
    | upperC == 'Y'     = Score 4
    | upperC == 'Z'     = Score 10
    | otherwise         = Score 0
    where upperC = C.toUpper c


scoreString :: String -> Score
scoreString = mconcat . map score



