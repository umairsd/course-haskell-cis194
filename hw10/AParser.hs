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

--newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- [Umair's note] this is my version, to clarify the concepts
newtype Parser a = Parser (String -> Maybe (a, String))
runParser (Parser f) = \str -> f str

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




--------------
-- Exercise 3
--------------
{-
You should implement each of the following exercises using the Applicative
interface to put together simpler parsers into more complex ones. Do not
implement them using the low-level definition of a Parser! In other words,
pre- tend that you do not have access to the Parser constructor or even know
how the Parser type is defined.
-}

-- Part A
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'
{-
-- Version using the low-level details
abParser = Parser out
    where
        p1 = char 'a'
        p2 = char 'b'
        out = \x1 -> case runParser p1 x1 of
            Nothing         -> Nothing
            Just (c1, x2)   -> case runParser p2 x2 of
                Nothing         -> Nothing
                Just (c2, y)    -> Just ((c1, c2), y)
-}

-- Part B
abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'


-- Part C
intPair :: Parser [Integer]
intPair = (\x _ z -> (x:z:[])) <$> posInt <*> char ' ' <*> posInt



--------------
-- Exercise 4
--------------

instance Alternative Parser where
    -- empty represents the parser that always fails
    -- empty :: f a
    empty       = Parser (\_ -> Nothing)
    -- p1 <|> p2 represents the parser which first tries p1. If p1 succeeds
    -- then p2 is ignored and the result of p1 is returned. Otherwise, if p1 fails
    -- then p2 is tried instead
    -- (<|>) :: f a -> f a -> f a
    (<|>) p1 p2 = Parser (\input -> case runParser p1 input of
                        Nothing     -> case runParser p2 input of
                                Nothing     -> Nothing
                                p2Result    -> p2Result
                        p1Result    -> p1Result)



--------------
-- Exercise 5
--------------

intOrUppercase :: Parser ()
-- intOrUppercase = (\_ -> ()) <$> posInt <|> (\_ -> ()) <$> (satisfy isUpper)
intOrUppercase = f posInt <|> f (satisfy isUpper)
    where f = fmap (\_ -> ())




