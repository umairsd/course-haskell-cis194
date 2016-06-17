{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee

--------------
-- Exercise 1
--------------

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = n}) (GL xs fun) = GL (emp:xs) (fun+n)


instance Monoid GuestList where
    mempty                        = GL [] 0
    mappend (GL xs n1) (GL ys n2) = GL (xs ++ ys) (n1 + n2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
    | gl1 < gl2  = gl2
    | otherwise = gl1


