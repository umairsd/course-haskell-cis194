{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/07-folds-monoids.pdf
-}


{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module JoinList where

import Sized
import Scrabble
import Buffer
import Editor



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




--------------
-- Exercise 3
--------------
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str




--------------
-- Exercise 4
--------------

-- class Buffer b where
instance Buffer (JoinList (Score, Size) String) where

    -- | Convert a buffer to a String.
    -- toString :: b -> String
    toString    = unlines . jlToList

    -- | Create a buffer from a String.
    -- fromString :: String -> b
    fromString  = foldr (+++) Empty . map (\str -> (Single (scoreString str, Size 1) str) ) . lines

    -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
    -- for out-of-bounds indices.
    -- line :: Int -> b -> Maybe String
    line = indexJ

    -- | @replaceLine n ln buf@ returns a modified version of @buf@,
    --   with the @n@th line replaced by @ln@.  If the index is
    --   out-of-bounds, the buffer should be returned unmodified.
    -- replaceLine :: Int -> String -> b -> b
    replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n+1) jl

    -- | Compute the number of lines in the buffer.
    -- numLines :: b -> Int
    numLines = getSize . snd . tag

    -- | Compute the value of the buffer, i.e. the amount someone would
    --   be paid for publishing the contents of the buffer.
    -- value :: b -> Int
    value = getScore . fst . tag



main = runEditor editor (fromString "HW7-Test" :: (JoinList (Score, Size) String) )


--------------
-- END
--------------



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
