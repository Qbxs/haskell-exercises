{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Exercises where

import Data.Kind (Type, Constraint)
import Data.Function ((&))
import Prelude hiding ((!!), tail, length, head)





{- ONE -}

-- | One of the restrictions around classes that we occasionally hit is that we
-- can only have one instance for a type. There are, for example, two good
-- candidates for a monoid instance when we think about 'Integer':

data IntegerMonoid = Sum | Product

-- | a. Write a newtype around 'Integer' that lets us choose which instance we
-- want.

newtype I (k :: IntegerMonoid) = I { unI :: Integer }

-- | b. Write the two monoid instances for 'Integer'.

instance Semigroup (I Sum) where
    x <> y = I $ unI x + unI y
instance Monoid (I Sum) where
  mempty = I 0

instance Semigroup (I Product) where
    x <> y = I $ unI x * unI y
instance Monoid (I Product) where
  mempty = I 1




-- | c. Why do we need @FlexibleInstances@ to do this?

-- Due to the restriction: (All instance types must be of the form (T a1 ... an)



{- TWO -}

-- | We can write a type that /is/ of kind 'Type', but has no value-level
-- members. We usually call this type 'Void':

data Void -- No constructors!

-- | a. If we promote this with DataKinds, can we produce any /types/ of kind
-- 'Void'?

-- | b. What are the possible type-level values of kind 'Maybe Void'?

-- | c. Considering 'Maybe Void', and similar examples of kinds such as
-- 'Either Void Bool', why do you think 'Void' might be a useful kind?





{- THREE -}

-- | a. Write a GADT that holds strings or integers, and keeps track of how
-- many strings are present. Note that you might need more than 'Nil' and
-- 'Cons' this time...

data Nat = Z | S Nat

data StringAndIntList (stringCount :: Nat) where
  Nil' :: StringAndIntList Z
  ConsInt :: Int -> StringAndIntList n -> StringAndIntList n
  ConsString :: String -> StringAndIntList n -> StringAndIntList (S n)

-- | b. Update it to keep track of the count of strings /and/ integers.

data StringAndIntList' (count :: Nat) where
  Nil'' :: StringAndIntList' Z
  ConsInt' :: Int -> StringAndIntList' n -> StringAndIntList' (S n)
  ConsString' :: String -> StringAndIntList' n -> StringAndIntList' (S n)

-- | c. What would be the type of the 'head' function?

-- | We could also return Either String Int
head :: StringAndIntList (S n) -> String
head (ConsInt n rest) = head rest
head (ConsString str _) = str

head' :: StringAndIntList' (S n) -> Either Int String
head' (ConsInt' n _) = Left n
head' (ConsString' str _) = Right str

{- FOUR -}

-- | When we talked about GADTs, we discussed existentials, and how we could
-- only know something about our value if the context told us:

data Showable where
  Showable :: Show a => a -> Showable

-- | a. Write a GADT that holds something that may or may not be showable, and
-- stores this fact in the type-level.

data MaybeShowable (isShowable :: Bool) where
  NotShowable :: a -> MaybeShowable False
  JustShowable :: Show a => a -> MaybeShowable True

-- | b. Write a 'Show' instance for 'MaybeShowable'. Your instance should not
-- work unless the type is actually 'show'able.

instance Show (MaybeShowable True) where
  show (JustShowable x) = show x

-- | c. What if we wanted to generalise this to @Constrainable@, such that it
-- would work for any user-supplied constraint of kind 'Constraint'? How would
-- the type change? What would the constructor look like? Try to build this
-- type - GHC should tell you exactly which extension you're missing.

data Constrainable (constraint :: Type -> Constraint) (isConstrainable :: Bool) where
  NotConstrainable :: a -> Constrainable constraint False
  JustConstrainable :: (constraint a) => a -> Constrainable constraint True



{- FIVE -}

-- | Recall our list type:

data List a = Nil | Cons a (List a)

-- | a. Use this to write a better 'HList' type than we had in the @GADTs@
-- exercise. Bear in mind that, at the type-level, 'Nil' and 'Cons' should be
-- "ticked". Remember also that, at the type-level, there's nothing weird about
-- having a list of types!

data HList (types :: List Type) where
  HNil  :: HList Nil
  HCons :: a -> HList b -> HList (Cons a b)

-- | b. Write a well-typed, 'Maybe'-less implementation for the 'tail' function
-- on 'HList'.

tail :: HList (Cons a as) -> HList as
tail (HCons _ xs) = xs

-- | c. Could we write the 'take' function? What would its type be? What would
-- get in our way?

-- the argument needs to depend on the List Type
-- it could work of instead of List we have a Vec with size and some constraint for the argument of take



{- SIX -}

-- | Here's a boring data type:

data BlogAction'
  = AddBlog'
  | DeleteBlog'
  | AddComment'
  | DeleteComment'

-- | a. Two of these actions, 'DeleteBlog' and 'DeleteComment', should be
-- admin-only. Extend the 'BlogAction' type (perhaps with a GADT...) to
-- express, at the type-level, whether the value is an admin-only operation.
-- Remember that, by switching on @DataKinds@, we have access to a promoted
-- version of 'Bool'!

data Role = Admin | Any | Mod

data BlogAction (r :: Role) where
  AddBlog :: BlogAction Any
  DeleteBlog :: BlogAction Admin
  AddComment :: BlogAction Any
  DeleteComment :: BlogAction Mod

-- | b. Write a 'BlogAction' list type that requires all its members to be
-- the same "access level": "admin" or "non-admin".

data BlogActionList (r :: Role) where
  BNil :: BlogActionList r
  BCons :: BlogAction r -> BlogActionList r -> BlogActionList r

newtype BlogActionList' (r :: Role) = BlogActionList' { getBlogActionList :: BlogAction r }

-- | c. Let's imagine that our requirements change, and 'DeleteComment' is now
-- available to a third role: moderators. Could we use 'DataKinds' to introduce
-- the three roles at the type-level, and modify our type to keep track of
-- this?

-- Yes. Add a new constructor to Permission and use it in AddComment.



{- SEVEN -}

-- | When we start thinking about type-level Haskell, we inevitably end up
-- thinking about /singletons/. Singleton types have a one-to-one value-type
-- correspondence - only one value for each type, only one type for each value.
-- A simple example is '()', whose only value is '()'. 'Bool' is /not/ a
-- singleton, because it has multiple values.

-- We can, however, /build/ a singleton type for 'Bool':

data SBool (value :: Bool) where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

-- | a. Write a singleton type for natural numbers:

data SNat (value :: Nat) where
  SZero :: SNat Z
  SSucc :: SNat n -> SNat (S n)

-- | b. Write a function that extracts a vector's length at the type level:

length :: Vector n a -> SNat n
length VNil = SZero
length (VCons _ xs) = SSucc (length xs)

-- | c. Is 'Proxy' a singleton type?

data Proxy a = Proxy

-- No, we have Proxy :: Proxy Nat, Proxy :: Proxy Bool, and so on



{- EIGHT -}

-- | Let's imagine we're writing some Industry Haskell™, and we need to read
-- and write to a file. To do this, we might write a data type to express our
-- intentions:

data Program                     result
  = OpenFile            (Program result)
  | WriteFile  String   (Program result)
  | ReadFile  (String -> Program result)
  | CloseFile (          Program result)
  | Exit                         result

-- | We could then write a program like this to use our language:

myApp :: Program Bool
myApp
  = OpenFile $ WriteFile "HEY" $ (ReadFile $ \contents ->
      if contents == "WHAT"
        then WriteFile "... bug?" $ Exit False
        else CloseFile            $ Exit True)

-- | ... but wait, there's a bug! If the contents of the file equal "WHAT", we
-- forget to close the file! Ideally, we would like the compiler to help us: we
-- could keep track of whether the file is open at the type level!
--
-- - We should /not/ be allowed to open a file if another file is currently
-- open.
--
-- - We should /not/ be allowed to close a file unless a file is open.
--
-- If we had this at the type level, the compiler should have been able to tell
-- us that the branches of the @if@ have different types, and this program
-- should never have made it into production. We should also have to say in the
-- type of 'myApp' that, once the program has completed, the file will be
-- closed.

-- | Improve the 'Program' type to keep track of whether a file is open.  Make
-- sure the constructors respect this flag: we shouldn't be able to read or
-- write to the file unless it's open. This exercise is a bit brain-bending;
-- why? How could we make it more intuitive to write?

data Program' result where
  OpenFile'  :: Program' result -> Program' result
  WriteFile' :: String -> Program' result -> Program' result
  ReadFile'  :: (String -> Program' result)  -> Program' result
  CloseFile' :: Program' result -> Program' result
  Exit'      :: result -> Program' result

-- | EXTRA: write an interpreter for this program. Nothing to do with data
-- kinds, but a nice little problem.

interpret :: Program {- ??? -} a -> IO a
interpret = error "Implement me?"





{- NINE -}

-- | Recall our vector type:

data Vector (n :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | Imagine we want to write the '(!!)' function for this vector. If we wanted
-- to make this type-safe, and avoid 'Maybe', we'd have to have a type that can
-- only hold numbers /smaller/ than some type-level value.

-- | a. Implement this type! This might seem scary at first, but break it down
-- into Z and S cases. That's all the hint you need :)

-- | The desired data type is the type of finite sets Fin.
data Fin (limit :: Nat) where
  FinZ :: Fin ('S n)
  FinS :: Fin n -> Fin ('S n)

-- | b. Write the '(!!)' function:

(!!) :: Vector n a -> Fin n -> a
(VCons x _) !! FinZ = x
(VCons x xs) !! (FinS n) = xs !! n

-- | c. Write a function that converts a @Fin n@ into a 'Nat'.

finToNat :: Fin n -> Nat
finToNat FinZ = S Z
finToNat (FinS n) = S (finToNat n)