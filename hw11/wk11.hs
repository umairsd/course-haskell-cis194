-- http://www.seas.upenn.edu/~cis194/spring13/lectures/11-applicative2.html
--
-- Implementing a few functions from the lecture notes
--

import Control.Applicative

-- This is my version of *>
pointRightStar :: Applicative f => f a -> f b -> f b
pointRightStar = Control.Applicative.liftA2 (const id)

{-|

f = Maybe
f = []
f = ZipList
f = IO
f = Parser

-}

mapA' :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA' g xs = sequenceA' $ map g xs
{-|

f = Maybe
f = []
f = ZipList
f = IO
f = Parser

-}



-- Reverse engineered based on the actual sequenceA function
--
sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' = foldr f (pure [])
  where
    f = \x acc -> (:) <$> x <*> acc

-- The version in Haskell
-- http://hackage.haskell.org/package/base-4.9.0.0/docs/src/Data.Traversable.html#sequenceA
--
{-|
    -- | Map each element of a structure to an action, evaluate these actions
    -- from left to right, and collect the results. For a version that ignores
    -- the results see 'Data.Foldable.traverse_'.
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f

    -- | Evaluate each action in the structure from left to right, and
    -- and collect the results. For a version that ignores the results
    -- see 'Data.Foldable.sequenceA_'.
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id
-}
{-|

f = Maybe
f = []
f = ZipList
f = IO
f = Parser

-}


replicateA' :: Applicative f => Int -> f a -> f [a]
replicateA' n x = sequenceA' $ replicate n x
{-|

f = Maybe
f = []
f = ZipList
f = IO
f = Parser

-}
