{-
CIS-194 (Spring 2013)

Source: http://www.cis.upenn.edu/~cis194/spring13/hw/01-intro.pdf
-}

-- {-# OPTIONS_GHC -Wall -fno-warn-unused-binds #-}

{-# OPTIONS_GHC -Wall #-}

-- Need a dummy module to handle warnings about undefined main, and unused
-- functions
module HW1 where


-- Exercise 1
-- Find the digits of a number
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0     = []
  | otherwise  = (n `mod` 10) : toDigitsRev (n `div` 10)


-- Exercise 2
-- Double every other digit from the right.
-- Start with the second-last, and so on
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherHelper . reverse

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper []       = []
doubleEveryOtherHelper [x]      = [x]
doubleEveryOtherHelper (x:y:xs) = x : (2 * y) : doubleEveryOtherHelper xs


-- Exercise 3
-- The output of doubleEveryOtherHelper has a mix of one digit and two
-- digit  numbers. This function calculates the sum of all digits
sumDigits :: [Integer] -> Integer
sumDigits  = sum . map (sum . toDigits)


-- Exercise 4
--
validate :: Integer -> Bool
validate n = total `mod` 10 == 0
  where total = (sumDigits . doubleEveryOther . toDigits) n


-- Exercise 5
-- Towers of Hanoi using 3 pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer
    -> Peg    -- start
    -> Peg    -- end
    -> Peg    -- temp
    -> [Move] -- list of moves
hanoi 0 _  _  _ = []
hanoi n ps pe pt = hanoi (n-1) ps pt pe ++ [(ps,pe)] ++ hanoi (n-1) pt pe ps



-- Exercise 6
-- Hanoi using 4 pegs
--
-- Solution from: https://www2.bc.edu/~grigsbyj/Rand_Final.pdf
hanoi4 :: Integer
    -> Peg -- start
    -> Peg -- end
    -> Peg -- intermediate 1
    -> Peg -- intermediate 2
    -> [Move]
hanoi4              = undefined





--
