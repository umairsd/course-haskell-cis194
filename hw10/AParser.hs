{- CIS 194 HW 10
   due Monday, 1 April
-}

{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------



--------------
-- Exercise 1
--------------

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y)= (f x, y)

instance Functor Parser where
    -- The types are as follows:
    -- fmap             :: (a->b) -> f a -> f b
    -- fmap             :: (a->b) -> Parser a -> Parser b
    --
    -- p                :: String -> Maybe (a, String)
    -- x                :: String
    -- runParser p x    :: Maybe (a, String)
    fmap f p = Parser (\ x -> case runParser p x of
                        Nothing  -> Nothing
                        Just y   -> Just (first f y) )


--------------
-- Exercise 2
--------------

instance Applicative Parser where
    -- pure :: a -> f a
    pure x = Parser (\str -> Just (x, str) )

    -- (<*>)            :: f (a -> b) -> f a -> f b
    -- p1               :: Parser( String -> Maybe (a->b, String) )
    -- p2               :: Parser( String -> Maybe (a, String) )
    -- output           :: Parser( String -> Maybe (b, String) )
    -- x1, x2, out      :: String
    -- runParser p1 x1  :: Maybe(a->b, String)
    -- runParser p2 x2  :: Maybe(a, String)
    -- fab              :: (a -> b)
    -- y                :: a
    --
    (<*>) p1 p2 = Parser result
        where
            result = \x1 -> case runParser p1 x1 of
                                Nothing         -> Nothing
                                Just (fab, x2)  -> case runParser p2 x2 of
                                        Nothing         -> Nothing
                                        Just(y, out)    -> Just (fab y, out)


