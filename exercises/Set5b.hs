-- Exercise set 5b: playing with binary trees

module Set5b where

import Mooc.Todo

-- The next exercises use the binary tree type defined like this:

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 1: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Empty)

valAtRoot :: Tree a -> Maybe a
valAtRoot Empty = Nothing
valAtRoot (Node a b c) = Just a 

------------------------------------------------------------------------------
-- Ex 2: compute the size of a tree, that is, the number of Node
-- constructors in it
--
-- Examples:
--   treeSize (Node 3 (Node 7 Empty Empty) Empty)  ==>  2
--   treeSize (Node 3 (Node 7 Empty Empty) (Node 1 Empty Empty))  ==>  3

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node a t1 t2) = let n1  = treeSize t1
                              n2 = treeSize t2
                          in n1 + (n2 + 1)


------------------------------------------------------------------------------
-- Ex 3: get the largest value in a tree of positive Ints. The
-- largest value of an empty tree should be 0.
--
-- Examples:
--   treeMax Empty  ==>  0
--   treeMax (Node 3 (Node 5 Empty Empty) (Node 4 Empty Empty))  ==>  5

treeMax :: Tree Int -> Int
treeMax Empty = 0
treeMax (Node n t1 t2) = let n1 = treeMax t1
                             n2 = treeMax t2
                         in max n (max n1 n2)
                          

------------------------------------------------------------------------------
-- Ex 4: implement a function that checks if all tree values satisfy a
-- condition.
--
-- Examples:
--   allValues (>0) Empty  ==>  True
--   allValues (>0) (Node 1 Empty (Node 2 Empty Empty))  ==>  True
--   allValues (>0) (Node 1 Empty (Node 0 Empty Empty))  ==>  False

allValues :: (a -> Bool) -> Tree a -> Bool
allValues condition Empty = True
allValues condition (Node n t1 t2) = let b1 = allValues condition t1
                                         b2 = allValues condition t2
                                         c3 = condition n
                                     in c3 && b1 && b2


------------------------------------------------------------------------------
-- Ex 5: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Empty  ==>  Empty
-- mapTree (+2) (Node 0 (Node 1 Empty Empty) (Node 2 Empty Empty))
--   ==> (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Node n t1 t2) = let f1 = mapTree f t1
                               f2 = mapTree f t2
                               fn = f n
                           in Node fn f1 f2
                              


------------------------------------------------------------------------------
-- Ex 6: given a value and a tree, build a new tree that is the same,
-- except all nodes that contain the value have been removed. Also
-- remove the subnodes of the removed nodes.
--
-- Examples:
--
--     1          1
--    / \   ==>    \
--   2   0          0
--
--  cull 2 (Node 1 (Node 2 Empty Empty)
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty))
--
--      1           1
--     / \           \
--    2   0   ==>     0
--   / \
--  3   4
--
--  cull 2 (Node 1 (Node 2 (Node 3 Empty Empty)
--                         (Node 4 Empty Empty))
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty)
--
--    1              1
--   / \              \
--  0   3    ==>       3
--   \   \
--    2   0
--
--  cull 0 (Node 1 (Node 0 Empty
--                         (Node 2 Empty Empty))
--                 (Node 3 Empty
--                         (Node 0 Empty Empty)))
--     ==> (Node 1 Empty
--                 (Node 3 Empty Empty))

cull :: Eq a => a -> Tree a -> Tree a
cull val Empty = Empty
cull val (Node n t1 t2) = let c1 = cull val t1
                              c2 = cull val t2
                          in if n == val then Empty
                             else Node n c1 c2
                                  
                                

------------------------------------------------------------------------------
-- Ex 7: check if a tree is ordered. A tree is ordered if:
--  * all values to the left of the root are smaller than the root value
--  * all of the values to the right of the root are larger than the root value
--  * and the left and right subtrees are ordered.
--
-- Hint: allValues will help you here!
--
-- Examples:
--         1
--        / \   is ordered:
--       0   2
--   isOrdered (Node 1 (Node 0 Empty Empty)
--                     (Node 2 Empty Empty))   ==>   True
--
--         1
--        / \   is not ordered:
--       2   3
--   isOrdered (Node 1 (Node 2 Empty Empty)
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        1     3   is not ordered:
--         \
--          0
--   isOrdered (Node 2 (Node 1 Empty
--                             (Node 0 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        0     3   is ordered:
--         \
--          1
--   isOrdered (Node 2 (Node 0 Empty
--                             (Node 1 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   True
--
largest Empty = Nothing 
largest (Node v t1 t2) = let a = largest t1
                             b = largest t2
                         in case (a,b) of
                              (Nothing ,  Nothing) -> Just v
                              (Just x  ,  Nothing) -> Just (max x v)
                              (Nothing ,  Just y)  -> Just (max y v)
                              (Just x  ,  Just y)  -> Just (max x (max y v))

smallest Empty = Nothing 
smallest (Node v t1 t2) = let a = smallest t1
                              b = smallest t2
                          in case (a,b) of
                               (Nothing ,  Nothing) -> Just v
                               (Just x  ,  Nothing) -> Just (min x v)
                               (Nothing ,  Just y)  -> Just (min y v)
                               (Just x  ,  Just y)  -> Just (min x (min y v))

 
leftNode Empty = Nothing
leftNode (Node _ t1 _) = Just t1

rightNode Empty = Nothing
rightNode (Node _ t1 _) = Just t1

topNode Empty = Nothing 
topNode (Node v _ _) = Just v

{--
 Node n t1 t2
 n must be larger than all of t1
 n must be smaller than all of t2
 then also -- must be recursively true isOrdered for leafs t1 t2 
--}
isOrdered :: Ord a => Tree a -> Bool
isOrdered Empty = True
isOrdered (Node n t1 t2) = let n1 = largest t1
                               n2 = smallest t2
                               o1 = isOrdered t1
                               o2 = isOrdered t2 
                           in case (n1,n2) of
                                (Nothing,Nothing) -> True
                                (Just v1,Nothing) -> n > v1 && o1 && o2 
                                (Nothing,Just v2) -> n < v2 && o1 && o2 
                                (Just v1,Just v2) -> (n > v1) && (n < v2) && o1 && o2 

-- tricky to see isOrdered also needs to be recursive 
                                
                                

------------------------------------------------------------------------------
-- Ex 8: a path in a tree can be represented as a list of steps that
-- go either left or right.

data Step = StepL | StepR
  deriving (Show, Eq)

-- Define a function walk that takes a tree and a list of steps, and
-- returns the value at that point. Return Nothing if you fall of the
-- tree (i.e. hit a Empty).
--
-- Examples:
--   walk [] (Node 1 (Node 2 Empty Empty) Empty)       ==>  Just 1
--   walk [StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Just 2
--   walk [StepL,StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Nothing

walk :: [Step] -> Tree a -> Maybe a
walk _ Empty = Nothing
walk [] (Node v _ _) = Just v
walk (StepL : _) (Node _ Empty _) = Nothing
walk (StepR : _) (Node _ _ Empty) = Nothing
walk (StepL : t) (Node _ t1 _) = walk t t1 
walk (StepR : t) (Node _ _ t2) = walk t t2

-- elegant indeed ! 




------------------------------------------------------------------------------
-- Ex 9: given a tree, a path and a value, set the value at the end of
-- the path to the given value. Since Haskell datastructures are
-- immutable, you'll need to build a new tree.
--
-- If the path falls off the tree, do nothing.
--
-- Examples:
--   set [] 1 (Node 0 Empty Empty)  ==>  (Node 1 Empty Empty)
--   set [StepL,StepL] 1 (Node 0 (Node 0 (Node 0 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--                  ==>  (Node 0 (Node 0 (Node 1 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--
--   set [StepL,StepR] 1 (Node 0 Empty Empty)  ==>  (Node 0 Empty Empty)


set :: [Step] -> a -> Tree a -> Tree a
set path val tree = set2 path val tree tree 
  where set2 _ _ Empty all = all 
        set2 []          val (Node v t1 t2) all = Node val t1 t2
        set2 (StepL : t) val (Node v t1 t2) all = Node v (set2 t val t1 t1) t2 
        set2 (StepR : t) val (Node v t1 t2) all = Node v t1 (set2 t val t2 t2) 

-- tricky if it runs off , we need to have entire tree available
-- if a sustitution is made , we can use that replacement and we are done 



------------------------------------------------------------------------------
-- Ex 10: given a value and a tree, return a path that goes from the
-- root to the value. If the value doesn't exist in the tree, return Nothing.
--
-- You may assume the value occurs in the tree at most once.
--
-- Examples:
--   search 1 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))  ==>  Just [StepL]
--   search 1 (Node 2 (Node 4 Empty Empty) (Node 3 Empty Empty))  ==>  Nothing
--   search 1 (Node 2 (Node 3 (Node 4 Empty Empty)
--                            (Node 1 Empty Empty))
--                    (Node 5 Empty Empty))                     ==>  Just [StepL,StepR]

search :: Eq a => a -> Tree a -> Maybe [Step]
search val tree = let path = []
                  in search2 val path tree

search2 val path Empty = Nothing
search2 val path (Node v t1 t2) =
  if v == val then Just path
  else let s1 = search2 val (path ++ [StepL]) t1
           s2 = search2 val (path ++ [StepR]) t2
       in case (s1,s2) of
            (Nothing, Nothing) -> Nothing
            (Just p , Nothing) -> Just p
            (Nothing, Just p2) -> Just p2
            (Just p , Just p2) -> Just p
                        
  
--- it worked , huh
-- ok . lets continue onwards and upwards



  

