{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cs194/spring13/hw/06-laziness.pdf
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE FlexibleInstances #-}

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
fibs2helper (f,s) = (s, f+s)


--------------
-- Exercise 3
--------------

data Stream a = Cons a (Stream a)


streamToList :: Stream a -> [a]
streamToList (Cons first rest)= first : streamToList rest


instance Show a => Show (Stream a) where
  show strm = foldr (\s acc -> show s ++ ", " ++ acc) "" $ take 20 $ streamToList strm



--------------
-- Exercise 4
--------------

-- generates a stream containing infinitely many copies of the given element
streamRepeat :: a -> Stream a
streamRepeat e = Cons e (streamRepeat e)

-- applies a function to every element of a stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons first rest) = Cons (f first) (streamMap f rest)

-- generates a stream from "seed" of type a, which is the first element of the
-- stream, and an "unfolding rule" of type a -> a which specifies how to
-- transform the seed into a new seed, to be used for generating the rest of the
-- stream
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f e = Cons e (streamFromSeed f (f e))

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
interleaveStreams (Cons z zs) (Cons y ys) = Cons z (Cons y (interleaveStreams zs ys) )



--------------
-- Exercise 6
--------------

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))



instance Num (Stream Integer) where
  -- fromInteger :: Integer -> a
  fromInteger n = Cons n (streamRepeat 0)
  -- negate :: a -> a
  negate (Cons first rest) = Cons (-first) (negate rest)
  --
  (+) (Cons firstA restA) (Cons firstB restB) = Cons (firstA + firstB) (restA + restB)
  --
  (*) (Cons firstA restA) b@(Cons firstB restB) =
     Cons (firstA * firstB) ((streamMap (*firstA) restB) + (restA * b))


{-|

Example usage:

*Fibonacci> x^4
0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
*Fibonacci> x^5
0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
*Fibonacci> (1+x)^5
1, 5, 10, 10, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
*Fibonacci> (1+x)^3
1, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
*Fibonacci> (1+x)^2
1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
*Fibonacci>  (x^2 + x + 3) * (x - 5)
-15, -2, -4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,

-}

instance Fractional (Stream Integer) where
  (/) a@(Cons firstA restA) b@(Cons firstB restB) =
    Cons (firstA `div` firstB) (streamMap (`div` firstB) (restA - ( a/b * restB) ))


fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)





