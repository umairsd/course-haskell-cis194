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
    | gl1 < gl2 = gl2
    | otherwise = gl1


--------------
-- Exercise 2
--------------


data Tree a = Node {
     rootLabel :: a,         -- label value
     subForest :: [Tree a]   -- zero or more child trees
}

-- Mirroring the types of treeFold on foldr
treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f acc Node{rootLabel = r, subForest = []}  = f r acc
treeFold f acc Node{rootLabel = r, subForest = xs}  =
    foldr (\y newAcc -> treeFold f newAcc y) (f r acc) xs


--------------
-- Exercise 3
--------------

-- Takes the 'boss' of the current subtree (say, Bob), and the second argument
-- is a list of the results for each subtree under Bob. Each result is a pair
-- of GuestsLists: the first GL is the best possible guest list with the boss
-- of that subtree, and the second is the best possible guest list without the
-- boss of that subtree
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e@Emp {empFun = n}   []      = (GL [e] n, mempty)
nextLevel e                    (g:gl)  = (glCons e maxGLWithoutBoss, maxGLWithBoss)
    where
        (maxGLWithBoss, maxGLWithoutBoss) = foldr (\x acc -> (moreFun (fst x) (fst acc), moreFun (snd x) (snd acc) )) g gl



--------------
-- Exercise 4
--------------

maxFun :: Tree Employee -> GuestList
maxFun = undefined


