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
histogram :: [Integer] -> String
histogram = unlines . histoHelper

-- Helper method that generates a list of Strings. Each string in the list
-- represents one level of the histogram
histoHelper :: [Integer] -> [String]
histoHelper xs = generate2DFromNumbers xs ++ ["==========", "0123456789"]

-- Given a list of numbers, generates a 2D vertical histogram. Each level
-- is represented by a string.
generate2DFromNumbers :: [Integer] -> [String]
generate2DFromNumbers xs = generate2DHelper (findMaxCount countsMap) countsMap []
  where
    countsMap = countNumbers xs


generate2DHelper :: Integer -> [(Integer, Integer)] -> [String] -> [String]
generate2DHelper currentLevel countsMap result
  | currentLevel == 0 = result
  | otherwise     = result ++ [xyz] ++ generate2DHelper (currentLevel - 1) countsMap result
      where
        xyz = foldr (\x acc -> if snd x >= currentLevel then '*':acc else ' ':acc) [] countsMap


-- Given a map between integers and their counts, this finds the highest count
-- (i.e. the count of number repeated the most times)
findMaxCount :: [(Integer, Integer)] -> Integer
findMaxCount = foldl (\acc x -> max acc (snd x)) 0


-- Given a list of integers, creates a mapping between the integer, and its
-- count in the list. This map is represented by a list of tuples
countNumbers :: [Integer] -> [(Integer, Integer)]
countNumbers xs =
  let countsMap = zip [0..9] [0,0..]
  in foldl (\acc x -> incrementForNum x acc) countsMap xs

-- Takes a countMap (a list of tuples, the first element is key, and second)
-- is the value) and a number. It increments the count (value) of the number
-- in the map
incrementForNum :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
incrementForNum _ [] = []
incrementForNum n counts@(x:xs)
  | n < 0       = counts
  | n > 9       = counts
  | n == fst x  = (fst x, snd x + 1) : xs
  | otherwise   = x : incrementForNum n xs






