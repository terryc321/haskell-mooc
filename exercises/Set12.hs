module Set12 where

import Data.Functor
import Data.Foldable
import Data.List
import Data.Monoid

import Mooc.Todo

-- before we get too weirded out this is just MAP function -- generalized called fmap 

------------------------------------------------------------------------------
-- Ex 1: Implement the function incrementAll that takes a functor
-- value containing numbers and increments each number inside by one.
--
-- Examples:
--   incrementAll [1,2,3]     ==>  [2,3,4]
--   incrementAll (Just 3.0)  ==>  Just 4.0

incrementAll :: (Functor f, Num n) => f n -> f n
incrementAll x = fmap (+ 1) x

{-- fmap is a structure preserving map but generalised generalized
-- does this mean anything ?

ok -- passed 
--}



------------------------------------------------------------------------------
-- Ex 2: Sometimes one wants to fmap multiple levels deep. Implement
-- the functions fmap2 and fmap3 that map over nested functors.
--
-- Examples:
--   fmap2 on [[Int]]:
--     fmap2 negate [[1,2],[3]]
--       ==> [[-1,-2],[-3]]
--   fmap2 on [Maybe String]:
--     fmap2 head [Just "abcd",Nothing,Just "efgh"]
--       ==> [Just 'a',Nothing,Just 'e']
--   fmap3 on [[[Int]]]:
--     fmap3 negate [[[1,2],[3]],[[4],[5,6]]]
--       ==> [[[-1,-2],[-3]],[[-4],[-5,-6]]]
--   fmap3 on Maybe [Maybe Bool]
--     fmap3 not (Just [Just False, Nothing])
--       ==> Just [Just True,Nothing]

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f expr = fmap (\x -> fmap f x) expr  

fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 f expr = fmap (\x -> fmap2 f x ) expr 

-- ok -- passed

------------------------------------------------------------------------------
-- Ex 3: below you'll find a type Result that works a bit like Maybe,
-- but there are two different types of "Nothings": one with and one
-- without an error description.
--
-- Implement the instance Functor Result

data Result a = MkResult a | NoResult | Failure String
  deriving Show

instance Functor Result where
  fmap f NoResult = NoResult
  fmap f (Failure s) = Failure s
  fmap f (MkResult a) = MkResult (f a)

{--
this is starting to get worrying
we are able to pass the type checker , pass tests , we have no idea what it means 
-- ok -- passed
--}




------------------------------------------------------------------------------
-- Ex 4: Here's a reimplementation of the Haskell list type. You might
-- remember it from Set6. Implement the instance Functor List.
--
-- Example:
--   fmap (+2) (LNode 0 (LNode 1 (LNode 2 Empty)))
--     ==> LNode 2 (LNode 3 (LNode 4 Empty))

data List a = Empty | LNode a (List a)
  deriving Show

instance Functor List where
  fmap f Empty = Empty
  fmap f (LNode x y) = LNode (f x) (fmap f y)

{-- again uneasy feeling why it passed -- why cant we talk about List inside
instance Functor List where
  fmap f Empty = Empty
  fmap f (LNode x (List y)) = LNode (f x) (fmap f (List y))  <<== wont work cant talk about List ? why not 

--} 
-- ok -- passed   

------------------------------------------------------------------------------
-- Ex 5: Here's another list type. This time every node contains two
-- values, so it's a type for a list of pairs. Implement the instance
-- Functor TwoList.
--
-- Example:
--   fmap (+2) (TwoNode 0 1 (TwoNode 2 3 TwoEmpty))
--     ==> TwoNode 2 3 (TwoNode 4 5 TwoEmpty)

data TwoList a = TwoEmpty | TwoNode a a (TwoList a)
  deriving Show

instance Functor TwoList where
  fmap f TwoEmpty = TwoEmpty
  fmap f (TwoNode x y z) = TwoNode (f x) (f y) (fmap f z )

-- ok -- passed


------------------------------------------------------------------------------
-- Ex 6: Count all occurrences of a given element inside a Foldable.
--
-- Hint: you might find some useful functions from Data.Foldable.
-- Check the docs! Or then you can just implement count directly.
--
-- Examples:
--   count True [True,False,True] ==> 2
--   count 'c' (Just 'c') ==> 1

count :: (Eq a, Foldable f) => a -> f a -> Int
count e xs = foldr (\n acc -> if n == e then acc + 1 else acc) 0 xs

-- ok -- passed 


------------------------------------------------------------------------------
-- Ex 7: Return all elements that are in two Foldables, as a list.
--
-- Examples:
--   inBoth "abcd" "fobar" ==> "ab"
--   inBoth [1,2] (Just 2) ==> [2]
--   inBoth Nothing [3]    ==> []

inBoth :: (Foldable f, Foldable g, Eq a) => f a -> g a -> [a]
inBoth fa fb = let x = toList fa
                   y = toList fb
               in filter (\ex -> elem ex y) x

-- ok -- passed                   

------------------------------------------------------------------------------
-- Ex 8: Implement the instance Foldable List.
--
-- Remember what the minimal complete definitions for Foldable were:
-- you should only need to implement one function.
--
-- After defining the instance, you'll be able to compute:
--   sum (LNode 1 (LNode 2 (LNode 3 Empty)))    ==> 6
--   length (LNode 1 (LNode 2 (LNode 3 Empty))) ==> 3

instance Foldable List where
  foldr f val Empty = val
  foldr f val (LNode x y) = let val2 = foldr f val y
                            in f x val2

-- more like a game of fitting jigsaw together
-- , type checker saying if piece fits or not                                
-- ok -- passed                                
                               

------------------------------------------------------------------------------
-- Ex 9: Implement the instance Foldable TwoList.
--
-- After defining the instance, you'll be able to compute:
--   sum (TwoNode 0 1 (TwoNode 2 3 TwoEmpty))    ==> 6
--   length (TwoNode 0 1 (TwoNode 2 3 TwoEmpty)) ==> 4

instance Foldable TwoList where
  foldr f val TwoEmpty = val
  foldr f val (TwoNode x y z) = let val2 = foldr f val z
                                in f x (f y val2)
                                   
-- ok -- passed   

------------------------------------------------------------------------------
-- Ex 10: (Tricky!) Fun a is a type that wraps a function Int -> a.
-- Implement a Functor instance for it.
--
-- Figuring out what the Functor instance should do is most of the
-- puzzle.

data Fun a = Fun (Int -> a)

runFun :: Fun a -> Int -> a
runFun (Fun f) x = f x

instance Functor Fun where
  fmap f g = Fun (\x -> f $ runFun g x)

-- again one it passes the type checker
-- its mostly right 
-- ok -- passed   
  
  
------------------------------------------------------------------------------
-- Ex 11: (Tricky!) You'll find the binary tree type from Set 5b
-- below. We'll implement a `Foldable` instance for it!
--
-- Implementing `foldr` directly for the Tree type is complicated.
-- However, there is another method in Foldable we can define instead:
--
--   foldMap :: Monoid m => (a -> m) -> Tree a -> m
--
-- There's a default implementation for `foldr` in Foldable that uses
-- `foldMap`.
--
-- Instead of implementing `foldMap` directly, we can build it with
-- these functions:
--
--   fmap :: (a -> m) -> Tree a -> Tree m
--   sumTree :: Monoid m => Tree m -> m
--
-- So your task is to define a `Functor` instance and the `sumTree`
-- function.
--
-- Examples:
--   using the [] Monoid with the (++) operation:
--     sumTree Leaf :: [a]
--       ==> []
--     sumTree (Node [3,4,5] (Node [1,2] Leaf Leaf) (Node [6] Leaf Leaf))
--       ==> [1,2,3,4,5,6]
--   using the Sum Monoid
--     sumTree Leaf :: Sum Int
--       ==> Sum 0
--     sumTree (Node (Sum 3) (Node (Sum 2) Leaf Leaf) (Node (Sum 1) Leaf Leaf))
--       ==> Sum 6
--
-- Once you're done, foldr should operate like this:
--   foldr (:) [] Leaf   ==>   []
--   foldr (:) [] (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))  ==>   [1,2,3]
--
--   foldr (:) [] (Node 4 (Node 2 (Node 1 Leaf Leaf)
--                                (Node 3 Leaf Leaf))
--                        (Node 5 Leaf
--                                (Node 6 Leaf Leaf)))
--      ==> [1,2,3,4,5,6]
--
-- The last example more visually:
--
--        .4.
--       /   \
--      2     5     ====>  1 2 3 4 5 6
--     / \     \
--    1   3     6

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap = todo

sumTree :: Monoid m => Tree m -> m
sumTree = todo

instance Foldable Tree where
  foldMap f t = sumTree (fmap f t)

------------------------------------------------------------------------------
-- Bonus! If you enjoyed the two last exercises (not everybody will),
-- you'll like the `loeb` function:
--
--   https://github.com/quchen/articles/blob/master/loeb-moeb.md
