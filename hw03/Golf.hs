{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
-}

{-# OPTIONS_GHC -Wall #-}

module Golf where


-- Exercise 1 (Hopscotch)
--
skips :: [a] -> [[a]]
-- Take a list of numbers from 1 to (length xs), which represents the
-- size of our jump (i.e. number of elements to skip)
-- For each jump-size n, we drop n elements from the list, so that the
-- first element of the list is the first element of our pattern, and then
-- call getNth
skips xs = map (\n -> getNth n (drop (n-1) xs) )  [1.. (length xs)]

-- Given an integer n, and a list, this function takes the first element
-- of the list, and then every nth element thereafter.
getNth :: Int   -- Number of steps to skip from the first element
      -> [a]    -- Input list
      -> [a]
getNth 0 _          = []
getNth _ []         = []
getNth n lst@(x:_)  = x : getNth n (drop n lst)



-- Exercise 2 (Local Maxima)
--
localMaxima :: [Integer] -> [Integer]
localMaxima []          = []
localMaxima [_]         = []
localMaxima [_, _]      = []
localMaxima (x:y:z:xs)  =
    if x < y && y > z
      then y : localMaxima (y:z:xs)
      else localMaxima (y:z:xs)



-- Exercise 3 (Histogram)
{-
The approach is: 
- append [0..9] to the input list, so that we have all possible numbers
- sort and then group
- convert the list to a list of tuples, which has each element and its count [(e, count)]
- create a string of stars for each element corresponding to its count.
- At this point, the we will have a 2D list of the form:
  [ ["***"],    -- 0
    [""],       -- 1
    ["******"]] -- 2

- Pad the list with spaces so that all the strings are the same length
- transpose, and then reverse the list to create the histogram.
- append the footer strings
- unlines to create one long string

In summary, this looks like:

unlines $ append footer $ reverse $ transpose $ starsStr $ [(n,count-1)] $ group $ sort $ input ++ [0,1,2,3,4,5,6,7,8,9]
-}
histogram :: [Integer] -> String
histogram input = (unlines . (++ ["==========","0123456789"]) . reverse . transpose) ys
  where
    xs        = getCountTuples (input ++ [0..9])
    maxStrLen = snd $ maximumBy (\t1 t2 -> compare (snd t1) (snd t2)) xs
    ys        = map (getStarStr maxStrLen) xs

-- Given a list of integers, it generates a tuple of counts for each number
getCountTuples :: [Integer] -> [(Integer, Integer)]
getCountTuples = map f1 . group. sort 
  where 
    f1 = \xs@(x:_) -> (x, toInteger $ length xs - 1)

-- Given a maximum string length, and a tuple of (number,count), generates a string of '*' followed
-- by spaces, up to the maximum length specified.
getStarStr :: Integer -> (Integer, Integer) -> String
getStarStr maxStrLen (_, n) = take numStar (repeat '*') ++ take numSpaces (repeat ' ')
  where 
    numStar   = fromInteger n
    numSpaces = fromInteger maxStrLen - numStar


