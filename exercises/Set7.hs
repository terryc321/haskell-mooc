-- Exercise set 7

module Set7 where

import Mooc.Todo
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Data.Semigroup
import GHC.RTS.Flags (ParFlags(setAffinity))

------------------------------------------------------------------------------
-- Ex 1: you'll find below the types Time, Distance and Velocity,
-- which represent time, distance and velocity in seconds, meters and
-- meters per second.
--
-- Implement the functions below.

data Distance = Distance Double
  deriving (Show,Eq)

data Time = Time Double
  deriving (Show,Eq)

data Velocity = Velocity Double
  deriving (Show,Eq)

-- velocity computes a velocity given a distance and a time
velocity :: Distance -> Time -> Velocity
velocity (Distance d) (Time t) = Velocity (d / t)

-- travel computes a distance given a velocity and a time
travel :: Velocity -> Time -> Distance
travel (Velocity v) (Time t) = Distance (v * t)

-- ok - passed --- 

------------------------------------------------------------------------------
-- Ex 2: let's implement a simple Set datatype. A Set is a list of
-- unique elements. The set is always kept ordered.
--
-- Implement the functions below. You might need to add class
-- constraints to the functions' types.
--
-- Examples:
--   member 'a' (Set ['a','b','c'])  ==>  True
--   add 2 (add 3 (add 1 emptySet))  ==>  Set [1,2,3]
--   add 1 (add 1 emptySet)  ==>  Set [1]

data Set a = Set [a]
  deriving (Show,Eq)

-- emptySet is a set with no elements
emptySet :: Set a
emptySet = Set [] 

-- member tests if an element is in a set
member :: Eq a => a -> Set a -> Bool
member e (Set []) = False
member e (Set (h : t)) =
  if e == h
  then True
  else member e (Set t)

-- add a member to a set
add :: Ord a => a -> Set a -> Set a
add e (Set []) = Set [e]
add e (Set h) = let r = addHelper e h
                in Set r
 where addHelper e [] = [e]
       addHelper e (h : t) = if e < h then e : h : t
                             else if e == h then h : t 
                                  else h : (addHelper e t)
                                  

------------------------------------------------------------------------------
-- Ex 3: a state machine for baking a cake. The type Event represents
-- things that can happen while baking a cake. The type State is meant
-- to represent the states a cake can be in.
--
-- Your job is to
--
--  * add new states to the State type
--  * and implement the step function
--
-- so that they have the following behaviour:
--
--  * Baking starts in the Start state
--  * A successful cake (represented by the Finished value) is baked
--    by first adding eggs, then adding flour and sugar (flour and
--    sugar can be added in which ever order), then mixing, and
--    finally baking.
--  * If the order of Events differs from this, the result is an Error cake.
--    No Events can save an Error cake.
--  * Once a cake is Finished, it stays Finished even if additional Events happen.
--
-- The function bake just calls step repeatedly. It's used for the
-- examples below. Don't modify it.
--
-- Examples:
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake]  ==>  Finished
--   bake [AddEggs,AddFlour,AddSugar,Mix,Bake,AddSugar,Mix]  ==> Finished
--   bake [AddFlour]  ==>  Error
--   bake [AddEggs,AddFlour,Mix]  ==>  Error

data Event = AddEggs | AddFlour | AddSugar | Mix | Bake
  deriving (Eq,Show)

-- added more states to track where we are in the cooking process 
data State = Start | Error | Finished | SEgg | SEggSugar | SEggFlour |
             SEggSugarFlour | SEggFlourSugar | Mixed 
  deriving (Eq,Show)


step :: State -> Event -> State
step Error _ = Error
step Finished _ = Finished 
step Start AddEggs = SEgg

step SEgg AddFlour = SEggFlour 
step SEgg AddSugar = SEggSugar

step SEggFlour AddSugar = SEggFlourSugar 
step SEggSugar AddFlour = SEggSugarFlour 

step SEggFlourSugar Mix = Mixed 
step SEggSugarFlour Mix = Mixed

step Mixed Bake = Finished

step _ _ = Error

-- all other cases are errors 
-- step Start _ = Error
-- step AddEgg _ = Error
-- step AddedFlour _ = Error
-- step AddedSugar _ = Error
-- step Mixed _ = Error

-- do not edit this
bake :: [Event] -> State
bake events = go Start events
  where go state [] = state
        go state (e:es) = go (step state e) es

------------------------------------------------------------------------------
-- Ex 4: remember how the average function from Set4 couldn't really
-- work on empty lists? Now we can reimplement average for NonEmpty
-- lists and avoid the edge case.
--
-- PS. The Data.List.NonEmpty type has been imported for you
--
-- Examples:
--   average (1.0 :| [])  ==>  1.0
--   average (1.0 :| [2.0,3.0])  ==>  2.0

average :: Fractional a => NonEmpty a -> a
average (v :| []) = v
average (v :| z) = let numer = (v + (foldr (+) 0 z))
                       denom = fromIntegral (1 + (length z))
                   in numer / denom

-- still dont quite understand why we need a fromIntegral ??
-- ok - passed                       


------------------------------------------------------------------------------
-- Ex 5: reverse a NonEmpty list.
--
-- PS. The Data.List.NonEmpty type has been imported for you

reverseNonEmpty :: NonEmpty a -> NonEmpty a
reverseNonEmpty (a :| []) = (a :| [])
reverseNonEmpty (a :| z) = let xs = a : z
                           in let (rh : rt) = reverse xs
                              in (rh :| rt) 

--- fairly quick 
--- ok -- passed                                  

                           

------------------------------------------------------------------------------
-- Ex 6: implement Semigroup instances for the Distance, Time and
-- Velocity types from exercise 1. The instances should perform
-- addition.
--
-- When you've defined the instances you can do things like this:
--
-- velocity (Distance 50 <> Distance 10) (Time 1 <> Time 2)
--    ==> Velocity 20

-- semigroup is just an associative operation
-- associativity definition
-- (a op b) op c = a op (b op c)
-- order in which we do op to either side , yields a result that can op to another ??
-- 1 + (2 + 3) = (1 + 2) + 3

-- class Semigroup a where
--   -- An associative operation.
--   (<>) :: a -> a -> a

-- data Distance = Distance Double
--   deriving (Show,Eq)
instance Semigroup Distance where
  (Distance a) <>  (Distance b) = Distance (a + b) 

-- data Time = Time Double
--   deriving (Show,Eq)
instance Semigroup Time where
  Time x <> Time y = Time (x + y) 

-- data Velocity = Velocity Double
--   deriving (Show,Eq)
instance Semigroup Velocity where
  Velocity x <> Velocity y = Velocity (x + y) 

-- data Sum a = Sum a
-- instance Num a => Semigroup (Sum a) where
--   Sum a <> Sum b  =  Sum (a+b)

-- data Product a = Product a
-- instance Num a => Semigroup (Product a) where
--   Product a <> Product b   =  Product (a*In Haskell, Kinds are like "types for types."

 {--
Int, Double, Bool have kind * (they are concrete types).

  Maybe, [] (lists), Either have kind * -> *
(they are type constructors that need a type to become concrete).b)

some figuring out about concrete type with kind *
eg Distance Double is a concrete type , does not need any more types to make it
--}

-- ok -- passed 

------------------------------------------------------------------------------
-- Ex 7: implement a Monoid instance for the Set type from exercise 2.
-- The (<>) operation should be the union of sets.
--
-- What's the right definition for mempty?
--
-- What are the class constraints for the instances?

-- mempty is the empty set
-- <> is union between sets 
instance Ord a => Semigroup (Set a) where
  (Set a) <>  (Set b) = Set (union a b)
    where union [] [] = []
          union [] z  = z
          union x  [] = x
          union (h : t) (h2 : t2) = if h < h2 then h : (union t (h2 : t2))
                                    else if h == h2 then h : (union t t2)
                                         else h2 : (union (h : t) t2)


instance Ord a => Monoid (Set a)  where
   mempty = Set [] 


{--
data Set a = Set [a]
  deriving (Show,Eq)

==== Theory ===== 

class Semigroup a where
  -- An associative operation.
  (<>) :: a -> a -> a


semigroup has an associative operator <> defined
 1+(2+3)=(1+2)+3

monoid is a semigroup with a neutral element
class Semigroup a => Monoid a where
  -- The neutral element
  mempty :: a

--} 


------------------------------------------------------------------------------
-- Ex 8: below you'll find two different ways of representing
-- calculator operations. The type Operation1 is a closed abstraction,
-- while the class Operation2 is an open abstraction.
--
-- Your task is to add:
--  * a multiplication case to Operation1 and Operation2
--    (named Multiply1 and Multiply2, respectively)
--  * functions show1 and show2 that render values of
--    Operation1 and Operation2 to strings
--
-- Examples:
--   compute1 (Multiply1 2 3) ==> 6
--   compute2 (Multiply2 2 3) ==> 6
--   show1 (Add1 2 3) ==> "2+3"
--   show1 (Multiply1 4 5) ==> "4*5"
--   show2 (Subtract2 2 3) ==> "2-3"
--   show2 (Multiply2 4 5) ==> "4*5"


-- operation 1 limited only to 1+2 , 3-4 , 5*6 cannot build complex expressions 
data Operation1 = Add1 Int Int
                | Subtract1 Int Int
                | Multiply1 Int Int 
  deriving Show

compute1 :: Operation1 -> Int
compute1 (Add1 i j) = i+j
compute1 (Subtract1 i j) = i-j
compute1 (Multiply1 i j) = i * j 

show1 :: Operation1 -> String
show1 (Add1 i j) = (show i) ++ "+" ++ (show j)
show1 (Subtract1 i j) = (show i) ++ "-" ++ (show j)
show1 (Multiply1 i j) = (show i) ++ "*" ++ (show j)


data Add2 = Add2 Int Int
  deriving Show
data Subtract2 = Subtract2 Int Int
  deriving Show
data Multiply2 = Multiply2 Int Int
  deriving Show

class Operation2 op where
  compute2 :: op -> Int

instance Operation2 Add2 where
  compute2 (Add2 i j) = i+j

instance Operation2 Subtract2 where
  compute2 (Subtract2 i j) = i-j

instance Operation2 Multiply2 where
  compute2 (Multiply2 i j) = i*j

class Print2 a where
  show2 :: a -> String

instance Print2 Int where
  show2 n = show n

instance Print2 Float where
  show2 n = show n

instance Print2 Double where
  show2 n = show n

instance Print2 Add2 where   
  show2 (Add2 i j) = (show i) ++ "+" ++ (show j)

instance Print2 Subtract2 where   
  show2 (Subtract2 i j) = (show i) ++ "-" ++ (show j)

instance Print2 Multiply2 where   
  show2 (Multiply2 i j) = (show i) ++ "-" ++ (show j)

-- show2 (Subtract1 i j) = (show1 i) ++ "-" ++ (show1 j)
-- show2 (Multiply1 i j) = (show1 i) ++ "*" ++ (show1 j)
-- show2 :: Operation2 op -> String
-- show2 x = show (compute1 x)

-- bit complex , one was just simple case , second needed type classes 
-- ok -- passed -- 


------------------------------------------------------------------------------
-- Ex 9: validating passwords. Below you'll find a type
-- PasswordRequirement describing possible requirements for passwords.
--
-- Implement the function passwordAllowed that checks whether a
-- password is allowed.
--
-- Examples:
--   passwordAllowed "short" (MinimumLength 8) ==> False
--   passwordAllowed "veryLongPassword" (MinimumLength 8) ==> True
--   passwordAllowed "password" (ContainsSome "0123456789") ==> False
--   passwordAllowed "p4ssword" (ContainsSome "0123456789") ==> True
--   passwordAllowed "password" (DoesNotContain "0123456789") ==> True
--   passwordAllowed "p4ssword" (DoesNotContain "0123456789") ==> False
--   passwordAllowed "p4ssword" (And (ContainsSome "1234") (MinimumLength 5)) ==> True
--   passwordAllowed "p4ss" (And (ContainsSome "1234") (MinimumLength 5)) ==> False
--   passwordAllowed "p4ss" (Or (ContainsSome "1234") (MinimumLength 5)) ==> True

data PasswordRequirement =
  MinimumLength Int
  | ContainsSome String    -- contains at least one of given characters
  | DoesNotContain String  -- does not contain any of the given characters
  | And PasswordRequirement PasswordRequirement -- and'ing two requirements
  | Or PasswordRequirement PasswordRequirement  -- or'ing
  deriving Show

passwordAllowed :: String -> PasswordRequirement -> Bool
passwordAllowed s (MinimumLength n) = length s >= n
passwordAllowed s (ContainsSome digits) = elem True (map (\d -> elem d s) digits)
passwordAllowed s (DoesNotContain digits) = not (elem True (map (\d -> elem d s) digits))
passwordAllowed s (And pr1 pr2) = (passwordAllowed s pr1)  && (passwordAllowed s pr2)
passwordAllowed s (Or pr1 pr2) = (passwordAllowed s pr1) || (passwordAllowed s pr2)

{--
λ> passwordAllowed "a" (MinimumLength 1)
True
λ> passwordAllowed "a" (MinimumLength 2)
False
λ> passwordAllowed "ab" (MinimumLength 2)
True
--}

-- ok --- passed --- 

------------------------------------------------------------------------------
-- Ex 10: a DSL for simple arithmetic expressions with addition and
-- multiplication. Define the type Arithmetic so that it can express
-- expressions like this. Define the functions literal and operation
-- for creating Arithmetic values.
--
-- Define two interpreters for Arithmetic: evaluate should compute the
-- expression, and render should show the expression as a string.
--
-- Examples:
--   evaluate (literal 3) ==> 3
--   render   (literal 3) ==> "3"
--   evaluate (operation "+" (literal 3) (literal 4)) ==> 7
--   render   (operation "+" (literal 3) (literal 4)) ==> "(3+4)"
--   evaluate (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> 6
--   render   (operation "*" (literal 3) (operation "+" (literal 1) (literal 1)))
--     ==> "(3*(1+1))"
--

data Arithmetic = Todo
  deriving Show

literal :: Integer -> Arithmetic
literal = todo

operation :: String -> Arithmetic -> Arithmetic -> Arithmetic
operation = todo

evaluate :: Arithmetic -> Integer
evaluate = todo

render :: Arithmetic -> String
render = todo
