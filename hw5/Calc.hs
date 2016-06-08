{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf
-}

{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

{-|
This is from ExprT, pasting here for reference
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

-}

--------------
-- Exercise 1
--------------
eval :: ExprT -> Integer
eval (Lit x)      = x
eval (Add e1 e2)  = eval e1 + eval e2
eval (Mul e1 e2)  = eval e1 * eval e2


--------------
-- Exercise 2
--------------
evalStr :: String -> Maybe Integer
evalStr str =
  case (parseExp Lit Add Mul str) of
    Nothing       -> Nothing
    Just e        -> Just (eval e)


--------------
-- Exercise 3
--------------
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id


--------------
-- Exercise 4
--------------

-- Integer: Works like the original calculator
instance Expr Integer where
  lit e     = e
  -- add e1 e2 = e1 + e2
  add       = (+)
  -- mul e1 e2 = e1 * e2
  mul       = (*)


-- Bool:
-- Every literal value less than or equal to 0 is interpreted
-- as False, and all positive Integers are interpreted as True;
-- “addition” is logical or, “multiplication” is logical and
instance Expr Bool where
  lit e  = e > 0
  add    = (||)
  mul    = (&&)


-- Defining newtypes for MinMax and Mod7, which will be used
-- in the subsquent parts of Exercise 4
newtype MinMax = MinMax Integer deriving (Show, Eq)
newtype Mod7   = Mod7 Integer deriving (Show, Eq)

-- MinMax:
-- “addition” is taken to be the max function, while
-- “multiplication” is the min function
instance Expr MinMax where
  lit                         = MinMax
  add (MinMax e1) (MinMax e2) = MinMax (max e1 e2)
  mul (MinMax e1) (MinMax e2) = MinMax (min e1 e2)


-- Mod7:
-- all values should be in the ranage 0 . . . 6, and
-- all arithmetic is done modulo 7; for example,
-- 5 + 3 = 1.
instance Expr Mod7 where
  lit e = Mod7 (e `mod` 7)
  add (Mod7 e1) (Mod7 e2) = lit (e1 + e2)
  mul (Mod7 e1) (Mod7 e2) = lit (e1 * e2)


-- Tests (from homework description)
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7



--------------
