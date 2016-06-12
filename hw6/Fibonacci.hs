{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cs194/spring13/hw/06-laziness.pdf
-}

{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

--------------
-- Exercise 1
--------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibs1 :: [Integer]
fibs1 = map fib [1..]


--------------
-- Exercise 2
--------------

fibs2 :: [Integer]
fibs2 = map fst $ iterate fibs2helper (0,1)

fibs2helper :: (Integer, Integer) -> (Integer, Integer)
fibs2helper (x,y) = (y, x+y)


--------------
-- Exercise 3
--------------

data Stream a = Cons a (Stream a)


streamToList :: Stream a -> [a]
streamToList (Cons x xs)= x : streamToList xs


instance Show a => Show (Stream a) where
  show strm = foldr (\x acc -> show x ++ ", " ++ acc) "" $ take 20 $ streamToList strm



--------------
-- Exercise 4
--------------

-- generates a stream containing infinitely many copies of the given element
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- applies a function to every element of a stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x remaining) = Cons (f x) (streamMap f remaining)

-- generates a stream from "seed" of type a, which is the first element of the
-- stream, and an "unfolding rule" of type a -> a which specifies how to
-- transform the seed into a new seed, to be used for generating the rest of the
-- stream
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

{-| Example:
>> streamFromSeed (+1) 1
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
 -}


 --------------
 -- Exercise 5
 --------------

-- infinite stream of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 1

-- stream that corresponds to the ruler function
-- Values:    0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
-- Indices:   1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16
-- nth element in the stream (assuming the first element corresponds to n=1) is
-- the largest power of 2 which evenly divides n
ruler :: Stream Integer
ruler = streamMap powerOf2 nats

powerOf2 :: Integer -> Integer
powerOf2 n
  | n == 0    = 0
  | even n    = 1 + powerOf2 (n `div` 2)
  | otherwise = 0


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys) )



