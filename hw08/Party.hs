{-
CIS-194 (Spring 2013)

Source:
http://www.seas.upenn.edu/~cis194/spring13/hw/08-IO.pdf
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree
import Data.List

--------------
-- Exercise 1
--------------

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = n}) (GL xs fun) = GL (emp:xs) (fun+n)


instance Monoid GuestList where
    mempty                        = GL [] 0
    mappend (GL xs n1) (GL ys n2) = GL (xs ++ ys) (n1 + n2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
    | f1 >= f2  = gl1
    | otherwise = gl2


--------------
-- Exercise 2
--------------

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f acc Node{rootLabel = r, subForest = sf}  = f r mappedList
    where
        -- Fold each tree in the subForest into b, thus giving a [b]
        mappedList = map (treeFold f acc) sf


{-|
-- A version of the treeFold without the accumulator.

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node {rootLabel = r, subForest = sf}) = f r (map (treeFold f) sf)

-}

--------------
-- Exercise 3
--------------

-- Takes the 'boss' of the current subtree (say, Bob), and the second argument
-- is a list of the results for each subtree under Bob. Each result is a pair
-- of GuestsLists: the first GL is the best possible guest list with the boss
-- of that subtree, and the second is the best possible guest list without the
-- boss of that subtree
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss  []  = (glCons boss mempty, mempty)
nextLevel boss  gl  = (glCons boss glWithoutSubBoss, glWithSubBoss)
    where
        -- Get 1st element of each tuple (i.e. guest list for each sub-division that
        -- includes that sub-division's boss), and concatenate them together
        glWithSubBoss    = mconcat $ map fst gl
        -- Get the 2nd element for each tuple (i.e. the list for each sub-division
        -- without that division's boss) and concatenate them together.
        glWithoutSubBoss = mconcat $ map snd gl



--------------
-- Exercise 4
--------------

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun glWithRoot glWithoutRoot
    where
        (glWithRoot, glWithoutRoot) = treeFold nextLevel (mempty, mempty) tree



--------------
-- Exercise 5
--------------

main :: IO()
main = do
    -- companyStr :: String
    companyStr <- readFile "company.txt"
    let tree        = read companyStr :: Tree Employee
        (GL xs fn)  = maxFun tree
    putStrLn $ "Total fun: " ++ show fn
    putStrLn $ unlines . sort $ map empName xs

