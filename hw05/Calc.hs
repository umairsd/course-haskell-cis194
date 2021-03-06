{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf
-}

{-# OPTIONS_GHC -Wall #-}

{-| Without TypeSynonymInstances we get the following error:
Illegal instance declaration for ‘Expr StackVM.Program’
  (All instance types must be of the form (T t1 ... tn)
   where T is not a synonym.
   Use TypeSynonymInstances if you want to disable this.)
In the instance declaration for ‘Expr StackVM.Program’
 -}
{-# LANGUAGE TypeSynonymInstances #-}
{- Without FlexibleInstances, we get the following compiler error
     Illegal instance declaration for ‘Expr Program’
      (All instance types must be of the form (T a1 ... an)
       where a1 ... an are *distinct type variables*,
       and each type variable appears at most once in the instance head.
       Use FlexibleInstances if you want to disable this.)
-}
{-# LANGUAGE FlexibleInstances #-}



module Calc where

import ExprT
import Parser
import qualified StackVM

import qualified Data.Map as M



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
-- Exercise 5
--------------

instance Expr StackVM.Program where
    lit x    = StackVM.PushI x : []
    add x y  = x ++ y ++ [StackVM.Add]
    mul x y  = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul


-- Test Function
-- stackVM takes a program and executes it
testHelper :: Maybe StackVM.Program -> String
testHelper Nothing     = "Not a valid program"
testHelper (Just p)    = case StackVM.stackVM p of
    Right x     -> show x
    Left  err   -> err


-- Compiles the provided string, and then tries to execute it on the
-- VM, and prints the output (or the error)
compileTest :: String -> String
compileTest = testHelper . compile


{- Example Usage of the Test function:
t1 = compileTest "2 + 3 * 4"
-- produces: "IVal 14"

t2 = compileTest "(2 + 3) * 4"
-- produces: "IVal 20"

t3 = compileTest "(((2 * 6) + 4) * 3)"
-- produces: "IVal 40"

t4 = compileTest "(2 * 6 + 4) * 3"
-- produces: "IVal 60"

t5 = compileTest "(2 * 6 + 4) 3"
-- produces: "Not a valid program"
-}





--------------
-- Exercise 6
--------------

-- Types which are instances of HasVars have some notion of
-- named variables
class HasVars a where
  var :: String -> a


data VarExprT = VarExprT String
                | VarExprTLit Integer
                | VarExprTAdd VarExprT VarExprT
                | VarExprTMul VarExprT VarExprT
                deriving (Show, Eq)


instance HasVars VarExprT where
  var = VarExprT

instance Expr VarExprT where
  lit = VarExprTLit
  add = VarExprTAdd
  mul = VarExprTMul


instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  {-| For lit:
  lit takes an integer, and returns a function that takes a map as input, and
  provides an integer as the output.

  In the code below, x :: M.Map String Integer, so the output is a function
  that takes a map, and returns the integer that was passed in as input
  -}
  lit x = \_ -> Just x

  {-| For add & mul:
  Both f and g are of type "something that takes a map and returns Maybe Integer"
  The output of add and mul is also of type "that takes a map, and returns Maybe Integer"

  At this point we know that the output is a function that takes a map, and returns
  a Maybe Integer. This means that in our lambda, x :: M.Map String Integer

  Now, the only part we need to figure out is how to use f and g, so that we can get
  the output as a Maybe Integer. So, we pass the input to the lambda (x) to both f
  and g, and when both return (Just a) & (Just b), we apply the operation (add or mul)
  to a & b and wrap the result in Just
  -}
  add f g = \x ->
            case f x of
              Nothing   -> Nothing
              (Just a)  -> case g x of
                             Nothing  -> Nothing
                             (Just b) -> Just (a + b)

  mul f g = \x ->
           case f x of
             Nothing   -> Nothing
             (Just a)  -> case g x of
                            Nothing  -> Nothing
                            (Just b) -> Just (a * b)



-- Test function from the homework
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs








