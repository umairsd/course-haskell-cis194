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

{- |
(Append s left right) contains two sub-trees. The total size s is
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
