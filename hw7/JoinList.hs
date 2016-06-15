{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/07-folds-monoids.pdf-}

{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized


{- |
data JoinListBasic a =
      Empty
    | Single a
    | Append (JoinListBasic a) (JoinListBasic a)
    deriving (Show, Eq)

--
jlbToList :: JoinListBasic a -> [a]
jlbToList Empty = []
jlbToList (Single a) = [a]
jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

-}

-- the m parameter will be used to track monoidal annotations to the
-- structure.
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)


--------------
-- Exercise 1
--------------

-- Append function for JoinLists taht yields a new JoinList whose monoidal
-- annotation is derived from those of the two arguments
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x `mappend` tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _)  = m




--------------
-- Exercise 2
--------------

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ n (Single s x)
    | (n+1) == rootSize = Just x
    | otherwise         = Nothing
    where rootSize = (getSize . size) s
{- | (Append s left right) contains two sub-trees. The total size s is
the sum of leftSize, and rightSize. So:

 -  If n falls between 0 and leftSize-1, then it means it falls in the left subtree.
    As we go left, the index range does not change
 -  If n >= leftCount && n < rootSize, then it means it falls in the right subtree.
    As we go right, the effective index in the rightSubtree would be (n - leftSize)
-}
indexJ n (Append s left right)
    | n < 0 || n > rootSize = Nothing
    | n < leftSize          = indexJ n left
    | otherwise             = indexJ newIndex right
    where
        leftSize  = (getSize . size . tag) left
        rootSize  = (getSize . size) s
        newIndex  = n - leftSize


-- B:
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty

dropJ n jl@(Single _ _)
    | n <= 0    = jl
    | otherwise = Empty

dropJ n jl@(Append s left right)
    | n <= 0        = jl
    | n >= rootSize = Empty
    -- Drop n elements from the left subtree, and combine it with the right tree
    | n < leftSize  = dropJ n left +++ right
    -- Drop all elements (= leftSie) in left, and n-leftSize elements in right.
    -- We need to explicitly drop all elements in left, and combine the result
    -- with the result of dropping (n-leftSize) elements on right. This is so that
    -- we can correctly compute the new size
    | otherwise     = dropJ leftSize left +++ dropJ (n - leftSize) right
    where
        leftSize  = (getSize . size . tag) left
        rootSize  = (getSize . size) s



-- C:
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty

takeJ n jl@(Single _ _)
    | n <= 0    = Empty
    | otherwise = jl

takeJ n jl@(Append s left right)
    | n <= 0        = Empty
    | n >= rootSize = jl
    -- We are only taking a handful of elements in the left tree
    | n < leftSize  = takeJ n left
    -- We are taking all elements from left subtree, and (n-leftSize) elements
    -- from the right subtree.
    | otherwise     = left +++ takeJ (n - leftSize) right
    where
        leftSize  = (getSize . size . tag) left
        rootSize  = (getSize . size) s

-- NOTE: In the above code for takeJ, we can combine the last two clauses for
-- takeJ n (Append ...). I've kept them separate for readability


-- Copied from assignment, for testing
-- Usage:
-- Invoking jlToList on ys below resuls in "yeah"
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2



--
{-\

-- This works in GHCI.
-- Type :{ and enter
-- Paste the below code
-- Type :} and enter

let xs =  (Append   (Product 210)
                    (Append (Product 30)
                            (Single (Product 5) 'y')
                            (Append (Product 6)
                                    (Single (Product 2) 'e')
                                    (Single (Product 3) 'a')) )
                    (Single (Product 7) 'h')
          )

-- ys is for a tree containing sizes (Exercise 2)
let ys =  (Append   (Size 4)
                    (Append (Size 3)
                          (Single (Size 1) 'y')
                          (Append (Size 2)
                                  (Single (Size 1) 'e')
                                  (Single (Size 1) 'a')) )
                    (Single (Size 1) 'h')
          )

let xs = Append (Size 2) (Single (Size 1) 'h') (Single (Size 1) 'e')
let ys = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')
-}



{-|
A few examples on the usage of Size:

*JoinList> let z = Size 8
*JoinList> negate z
Size (-8)
*JoinList> let x = Size 5
*JoinList> x - z
Size (-3)
*JoinList> z - x
Size 3


-}
