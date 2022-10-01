{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where

import Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.





{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

type family (+) (x :: Nat) (y :: Nat) :: Nat where
  (+) Z y = y
  (+) (S x) y = S (x + y)

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

type family (**) (x :: Nat) (y :: Nat) :: Nat where
  (**) Z m = Z
  (**) (S n) m = m + (n ** m)

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | c. Write a function to add two 'SNat' values.

add :: SNat x -> SNat y -> SNat (x + y)
add SZ m = m
add (SS n) m = SS (add n m)



{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append VNil ys = ys
append (VCons x xs) ys = VCons x (append xs ys)

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
flatMap VNil f = VNil
flatMap (VCons x xs) f = f x `append` flatMap xs f





{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

type family (&&) (x :: Bool) (y :: Bool) :: Bool where
  (&&) True True = True
  (&&) _ _ = False

-- | b. Write the type-level @||@ function for booleans.

type family (||) (x :: Bool) (y :: Bool) :: Bool where
  (||) False False = False
  (||) _ _ = True

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.

type family All (x :: [Bool]) :: Bool where
  All [] = True
  All (True:xs) = All xs
  All (False:_) = False



{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.

type family Compare (x :: Nat) (y :: Nat) :: Ordering where
  Compare Z Z = EQ
  Compare (S _) Z = GT
  Compare Z (S _) = LT
  Compare (S n) (S m) = Compare n m

-- | b. Write a 'Max' family to get the maximum of two natural numbers.

type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max Z Z = Z
  Max (S n) Z = S n
  Max Z (S m) = S m
  Max (S n) (S m) = S (Max n m)

-- | c. Write a family to get the maximum natural in a list.

type family MaxList (xs :: [Nat]) :: Nat where
  MaxList [x] = x
  MaxList (x:xs) = Max x (MaxList xs)



{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.





{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.





{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.





{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  -- ...

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?





{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?
