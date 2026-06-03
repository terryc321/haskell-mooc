-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold. The function countNothings from
-- the course material can be implemented using foldr. Your task is to
-- define countHelper so that the following definition of countNothings
-- works.
--
-- Hint: You can start by trying to add a type signature for countHelper.
--
-- Challenge: look up the maybe function and use it in countHelper.
--
-- Examples:
--   countNothings []  ==>  0
--   countNothings [Just 1, Nothing, Just 3, Nothing]  ==>  2

countNothings :: [Maybe Int] -> Int
countNothings xs = foldr (countHelper) 0 xs

--- ok i get it , count the nothings 
countHelper :: Maybe Int -> Int -> Int
countHelper s acc = case s of
                      Nothing -> acc + 1
                      Just v -> acc 
              

------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = foldr maxHelper x xs

--maxHelper s acc = if s > acc then s else acc
maxHelper = max 

------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
--   sumAndLength []             ==>  (0.0,0)
--   sumAndLength [1.0,2.0,4.0]  ==>  (7.0,3)


sumAndLength :: [Double] -> (Double,Int)
sumAndLength xs = foldr slHelper slStart xs

slStart = (0.0,0)
slHelper s acc = let (tot,n) = acc
                 in (tot + s , n + 1) 

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

myConcat :: [[a]] -> [a]
myConcat xs = foldr concatHelper concatStart xs

concatStart = []
concatHelper s acc = s ++ acc 

------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

largest :: [Int] -> [Int]
largest xs = foldr largestHelper [] xs

largestHelper s [] = [s]
largestHelper s (h : t) = if s > h then [s]
                          else if s == h then s : h : t
                               else h : t
                                    



------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
--
-- Examples:
--   myHead []  ==>  Nothing
--   myHead [1,2,3]  ==>  Just 1

myHead :: [a] -> Maybe a
myHead xs = foldr headHelper Nothing xs

headHelper :: a -> Maybe a -> Maybe a 
headHelper = \ a b -> Just a
  
-- headHelper v Nothing = Just v
-- headHelper v _ = Just v

-- foldr (#) u [x1, x2, ..., xn] = x1 # (x2 # (...(xn # u)...))
-- in the tree
-- winds up being right leaning tree
--   #
--  / \
-- 1   #
--    / \
--   2   u   where foldr (#) u  , if u always picks first value , get leaf 1 eventually
-- more work , why not just head ?
-- let # = \ a b = a 


------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
--
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

myLast :: [a] -> Maybe a
myLast xs = foldr lastHelper Nothing xs

-- where we pick which leg to take 
--   # 1 (Just 2) --> Just 2 
--  / \
-- 1   # --> Just 2
--    / \
--   2  Init   where foldr (#) Init  ,  Initial value Init 

lastHelper :: a -> Maybe a -> Maybe a 
lastHelper a Nothing = Just a  -- bottom right leg 
lastHelper a (Just v) = Just v 

