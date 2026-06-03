-- Exercise set 4a:
--
--  * using type classes
--  * working with lists
--
-- Type classes you'll need
--  * Eq
--  * Ord
--  * Num
--  * Fractional
--
-- Useful functions:
--  * maximum
--  * minimum
--  * sort

module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--   allEqual [] ==> True
--   allEqual [1,2,3] ==> False
--   allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (h : t) = helper h t
 where helper h [] = True
       helper h (h2 : t) = if h == h2 then helper h t
                           else False
                                



------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
--
-- Hint: a certain function from the lecture material can make this
-- really easy for you.
--
-- Examples:
--   distinct [] ==> True
--   distinct [1,1,2] ==> False
--   distinct [1,2] ==> True

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct [x] = True
distinct (h : t) = let out = helper h t
  in if out then distinct t
     else False 
 where helper h [] = True
       helper h (h2 : t) = if h /= h2 then helper h t
                           else False 

------------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
--
-- The function should work on all types in the Ord class. Give it a
-- suitable type signature.
--
-- Examples:
--   middle 'b' 'a' 'c'  ==> 'b'
--   middle 1 7 3        ==> 3
-- a b c
-- a c b
-- b a c
-- b c a
-- c a b
-- c b a 

middle :: Ord a => a -> a -> a -> a 
middle a b c = (sort [a,b,c]) !! 1 
  

------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
--
-- Your function should work on all suitable types, like Float and
-- Int. You'll need to add _class constraints_ to the type of range.
--
-- It's fine if your function doesn't work for empty inputs.
--
-- Examples:
--   rangeOf [4,2,1,3]          ==> 3
--   rangeOf [1.5,1.0,1.1,1.2]  ==> 0.5

rangeOf :: (Num a, Ord a)  => [a] -> a
rangeOf xs =
  let s = sort xs
  in let low = head s
         high = last s
     in (high - low)
        
           
                

------------------------------------------------------------------------------
-- Ex 5: given a (non-empty) list of (non-empty) lists, return the longest
-- list. If there are multiple lists of the same length, return the list that
-- has the smallest _first element_.
--
-- (If multiple lists have the same length and same first element,
-- you can return any one of them.)
--
-- Give the function "longest" a suitable type.
--
-- Challenge: Can you solve this exercise without sorting the list of lists?
--
-- Examples:
--   longest [[1,2,3],[4,5],[6]] ==> [1,2,3]
--   longest ["bcd","def","ab"] ==> "bcd"

longest xs = helper xs []
 where helper [] r = r
       helper (h : t) r =
         if length h > length r then helper t h
         else if length h == length r then if (head h) < (head r) then helper t h
                                           else helper t r
         else helper t r
         
  
------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add _class constraints_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

incrementKey :: (Ord k , Num v) => k -> [(k,v)] -> [(k,v)]
incrementKey k [] = []
incrementKey k ((k2,v) : t) = if k == k2 then (k,v+1) : incrementKey k t
                              else (k2,v) : incrementKey k t

                                   

------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

average :: Fractional a => [a] -> a
average xs = let s = sum xs
                 l = length xs
             in (s / (fromIntegral l))

-- not sure what the holdup was here                 
-- let x = [1.2 / 2 , 2.2/ 3 , 3.3 / 4] in (sum x / (fromIntegral (length x)))
-- 0.7194444444444444
-- let tot = 0
--                  count = 0
--              in helper xs tot count
--   where helper [] 0 count = 0
--         helper [] tot ct = tot / ct
--         helper (h : t) tot ct = helper t (tot + h) (ct + 1)
          

------------------------------------------------------------------------------
-- Ex 8: given a map from player name to score and two players, return
-- the name of the player with more points. If the players are tied,
-- return the name of the first player (that is, the name of the
-- player who comes first in the argument list, player1).
--
-- If a player doesn't exist in the map, you can assume they have 0 points.
--
-- Hint: Map.findWithDefault can make this simpler
--
-- Examples:
--   winner (Map.fromList [("Bob",3470),("Jane",2130),("Lisa",9448)]) "Jane" "Lisa"
--     ==> "Lisa"
--   winner (Map.fromList [("Mike",13607),("Bob",5899),("Lisa",5899)]) "Lisa" "Bob"
--     ==> "Lisa"
-- Map.lookup s map => Nothing | Just v 
winner :: Map.Map String Int -> String -> String -> String
winner scores player1 player2 =
  case (Map.lookup player1 scores , Map.lookup player2 scores ) of
    (Just v1 , Just v2) -> if v1 >= v2 then player1
                           else player2
    (Nothing , Just v2) -> player2
    (Just v1 , Nothing) -> player1
    (Nothing,Nothing) -> player1
    

------------------------------------------------------------------------------
-- Ex 9: compute how many times each value in the list occurs. Return
-- the frequencies as a Map from value to Int.
--
-- [X] Challenge 1: try using Map.alter for this
--
-- [X] Challenge 2: use foldr to process the list
--
-- Example:
--   freqs [False,False,False,True]
--     ==> Map.fromList [(False,3),(True,1)]
-- list of things then make a map how many times it occurs 
freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int
-- freqs xs = helper xs (Map.fromList [])
--   where helper [] m = m
--         helper (h : t) m = helper t (Map.alter (\s -> case s of
--                                                    Nothing -> Just 1
--                                                    Just v -> Just (v + 1)) h m)
freqs xs = foldr (\x m -> Map.alter foo x m) Map.empty xs
  where foo s = case s of 
                  Nothing -> Just 1
                  Just v -> Just (v + 1)




------------------------------------------------------------------------------
-- Ex 10: recall the withdraw example from the course material. Write a
-- similar function, transfer, that transfers money from one account
-- to another.
--
-- However, the function should not perform the transfer if
-- * the from account doesn't exist,
-- * the to account doesn't exist,
-- * the sum is negative,
-- * or the from account doesn't have enough money.
--
-- Hint: there are many ways to implement this logic. Map.member or
-- Map.notMember might help.
--
-- Examples:
--   let bank = Map.fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Mike" 20 bank
--     ==> fromList [("Bob",80),("Mike",70)]
--   transfer "Bob" "Mike" 120 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Lisa" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]





--   transfer "Lisa" "Mike" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]

transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank =
  case (Map.lookup from bank, Map.lookup to bank) of
    (Just v1 , Just v2) -> let sum1 = v1 - amount
                               sum2 = v2 + amount
                           in if amount < 0 || sum1 < 0 then bank
                              else Map.insert from sum1 (Map.insert to sum2 bank)
    (_ , _) -> bank

    
                                   

------------------------------------------------------------------------------
-- Ex 11: given an Array and two indices, swap the elements in the indices.
--
-- Example:
--   swap 2 3 (array (1,4) [(1,"one"),(2,"two"),(3,"three"),(4,"four")])
--         ==> array (1,4) [(1,"one"),(2,"three"),(3,"two"),(4,"four")]

swap :: Ix i => i -> i -> Array i a -> Array i a
swap i j arr = let ati = arr ! i
                   atj = arr ! j
               in arr // [(i , atj),(j , ati)]

------------------------------------------------------------------------------
-- Ex 12: given an Array, find the index of the largest element. You
-- can assume the Array isn't empty.
--
-- You may assume that the largest element is unique.
--
-- Hint: check out Data.Array.indices or Data.Array.assocs
-- maxIndexHelpRec :: (Ix i, Ord a) => Array i a -> i -> i -> a -> i
-- maxIndexHelpRec arr lo hi elem i =
--           if lo > hi then i -- want index not the actual thing in array at index i ! 
--           else let alo = arr ! lo
--                in if alo == max alo elem 
--                   then maxIndexHelpRec arr (lo + 1) hi alo lo
--                   else maxIndexHelpRec arr (lo + 1) hi elem i

-- -- maxIndexHelp :: (Ix i, Ord a) => Array i a -> i -> i -> i
-- maxIndexHelp arr lo hi = let elem = arr ! lo
--                              i = lo
--                          in maxIndexHelpRec arr lo hi elem i 

-- myArr = (array (1,5) [(1,1),(2,2),(3,3),(4,4),(5,5)]) :: Array Int Int
-- Array.assocs == translate array into a list - use list processing as normal 
                       
maxIndex :: (Ix i, Ord a) => Array i a -> i
maxIndex arr = let ass = assocs arr
               in helper ass (head ass)
  where helper [] (i,v) = i
        helper ((ai,vi) : t) (i,v) = if vi > v then helper t (ai,vi)
                                     else helper t (i,v)

                                          



                  

-- 
-- λ> let a = array (1,10) ((1,1) : [(i, i * a Array.! (i-1)) | i <- [2..10]])
-- λ> maxIndex a 
-- 10
-- λ> a
-- array (1,10) [(1,1),(2,2),(3,6),(4,24),(5,120),(6,720),(7,5040),(8,40320),(9,362880),(10,3628800)]
-- λ> maxIndex a
-- 10
-- λ> 



                       

                       
                       

                


