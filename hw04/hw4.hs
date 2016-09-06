{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf
-}

{-# OPTIONS_GHC -Wall #-}

module HW4 where

-- Exercise 1
-- Rewrite the functions in wholemeal style
{-|
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs
-}
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

{-|
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-}
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate f2Help

f2Help :: Integer -> Integer
f2Help n
  | even n = n `div` 2
  | otherwise = 3*n + 1




-- Exercise 2: Folding with trees

-- Each node stores the height
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

-- Generate a balanced binary tree from a list of values using foldr
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf


insert :: a -> Tree a -> Tree a
insert x Leaf  = Node 0 Leaf x Leaf
insert x (Node h left v right)
  | height left < height right          = Node h (insert x left) v right
  | height left > height right          = Node h left v (insert x right)
  | countNodes left < countNodes right  = Node h (insert x left) v right
  | countNodes left > countNodes right  = Node h left v (insert x right)
  | otherwise                           = Node newHeight leftTree v right
                                    where leftTree  = insert x left
                                          newHeight = 1 + max (height leftTree) (height right)


-- insertIntoTree x (Node h l v r)    = Node newHeight left v right
--   where
--     left = if height l < height r
--       then insertIntoTree x l
--       else l
--     right = if height l < height r
--       then r
--       else insertIntoTree x r
--     maxChildHeight = max (height left) (height right)
--     newHeight = max h (maxChildHeight + 1)
--


-- Helper function to get the height of a given tree
height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h

-- Helper function that counts the nodes in a Tree
countNodes :: Tree a -> Integer
countNodes Leaf                   = 0
countNodes (Node _ left _ right)  = 1 + countNodes left + countNodes right


{-|
When foldTree is invoked via foldTree "ABCDEFGHIJ", we get the following output:

Node 3
  (Node 2
    (Node 0 Leaf 'C' Leaf)
    'H'
    (Node 1 Leaf 'F' (Node 0 Leaf 'B' Leaf)))
  'J'
  (Node 2
    (Node 1 Leaf 'E' (Node 0 Leaf 'A' Leaf))
    'I'
    (Node 1 Leaf 'G' (Node 0 Leaf 'D' Leaf)))
-}

-- Given the maximum height of the tree and its height, converts the tree to a
-- String
stringify :: Show a => Integer -> Tree a -> [String]
stringify _ Leaf = []
stringify n (Node h left x right) = [replicate m '-' ++ show x] ++ stringify n left ++ stringify n right
  where m = fromIntegral (n-h)





-- Exercise 3: More folds
-- A: Returns True iff there are odd number of True values
xor :: [Bool] -> Bool
xor = foldr xorHelper False

xorHelper :: Bool -> Bool -> Bool
xorHelper x y = (x && not y) || (not x && y)


-- B: Map using fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []


-- C: Implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base xs




-- Exercise 4: Finding Primes (Sieve of Sundaram)
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) . filter (not . (`elem` exclusionList) ) $ [1..n]
  where
    exclusionList = [i+j+2*i*j | i <- [1..n], j <- [1..n], i <= j]
















