{-# LANGUAGE GADTs, FlexibleInstances, FlexibleContexts #-}
module Exercises where
import Data.Foldable





{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  MkCountableList :: Countable a => [a] -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList (MkCountableList l) = sum $ map count l


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero (MkCountableList l) = MkCountableList $ filter ((== 0) . count) l


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "No, for type level programming we would need Type Families"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  MkAnyList :: [a] -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList (MkAnyList l) = MkAnyList $ reverse l

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList p (MkAnyList l) = error "because a is hidden in AnyList it cannot be unified with the a in the predicate" -- MkAnyList $ filter p l

lengthAnyList :: AnyList -> Int
lengthAnyList (MkAnyList l) = length l

foldAnyList :: Monoid m => AnyList -> m
foldAnyList (MkAnyList l) = error "constraint Monoid m can not be resolved" -- fold l

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList (MkAnyList l) = null l

instance Show AnyList where
  show = error "We can not constrain type var a in AnyList with Show a"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- input is existential
-- we can only apply the transform function to it

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance (Eq output) => Eq (TransformableTo output) where
  (TransformWith f x) == (TransformWith g y) = f x == g y

-- we basically check for functional extensionality here
-- because input is hidden and unconstrained we cannot check for the equality

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap f (TransformWith tr x) = TransformWith f (tr x)


{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

eqPairEqual :: EqPair -> Bool
eqPairEqual (EqPair x y) = x == y

eqPairDiffer :: EqPair -> Bool
eqPairDiffer = not . eqPairEqual

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- We would still need a GADT because otherwise we cannot constrain a to Eq a



{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _str (IntBox n EmptyBox)) = n

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 0
countLayers (IntBox _n b) = 1 + countLayers b -- = 1
countLayers (StringBox _str b) = 1 + countLayers b -- = 2
countLayers (BoolBox _bool b) = 1 + countLayers b -- = 3
-- we might as well do this in constant time without recursion because there is no arbitrary nesting

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

removeLayer :: MysteryBox a -> MysteryBox b
removeLayer _ = error "Not possible because we cannot remove a layer from EmptyBox, also we cannot write a type for this function (without type families)"



{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

hHead :: HList (a, b) -> a
hHead (HCons x _xs) = x

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe _ = error "no idea"

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- generally this is not possible because we need to know the type of each element, particularily of the last element of the first lists



{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  Nil :: HTree Empty
  Node :: HTree b -> a -> HTree c -> HTree (Branch b a c)

tree :: HTree (Branch Empty Integer (Branch Empty String Empty))
tree = Node Nil 1 (Node Nil "hello" Nil)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeftBranch :: HTree (Branch b a c) -> HTree (Branch Empty a c)
deleteLeftBranch (Node _l x r) = Node Nil x r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  Nil == Nil = True

instance (Eq  a, Eq (HTree b), Eq (HTree c)) => Eq (HTree (Branch b a c)) where
  (Node l x r) == (Node l' x' r') = l == l' && x == x' && r == r'


{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: AlternatingList a b
  ACons :: a -> AlternatingList b a -> AlternatingList a b

f :: AlternatingList Bool Int
f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts ANil = []
getFirsts (ACons x ys) = x : getSeconds ys

getSeconds :: AlternatingList a b -> [b]
getSeconds ANil = []
getSeconds (ACons _y xs) = getFirsts xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues alts = (fold $ getFirsts alts, fold $ getSeconds alts)

foldValues' :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues' ANil = mempty
foldValues' (ACons x ys) = let (b,a) = foldValues' ys
                            in (x <> a,b)



{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals n m) = eval n == eval m
eval (Add n m) = eval n + eval m
eval (If b x y) = if eval b then eval x else eval y
eval (IntValue n) = n
eval (BoolValue b) = b

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse (DirtyEquals n m) = Nothing
parse (DirtyAdd n m) = do
  n <- parse n
  m <- parse m
  pure (Add n m)
parse (DirtyIf b x y) = do
  b <- parseBool b
  x <- parse x
  y <- parse y
  pure (If b x y)
parse (DirtyIntValue n) = pure (IntValue n)
parse (DirtyBoolValue _b) = Nothing

parseBool :: DirtyExpr -> Maybe (Expr Bool)
parseBool (DirtyEquals n m) = do
  n <- parse n
  m <- parse m
  pure (Equals n m)
parseBool (DirtyAdd n m) = Nothing
parseBool (DirtyIf b x y) = do
  b <- parseBool b
  x <- parseBool x
  y <- parseBool y
  pure (If b x y)
parseBool (DirtyIntValue _n) = Nothing
parseBool (DirtyBoolValue b) = pure (BoolValue b)

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?

-- TODO



{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TNil :: TypeAlignedList a a
  TCons :: (a -> c) -> TypeAlignedList c b -> TypeAlignedList a b

example :: TypeAlignedList [a] Bool
example = TCons length (TCons even (TCons not TNil))

-- | b. Which types are existential?

-- c is existential

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs fs TNil = fs
composeTALs fs (TCons g gs) = TCons g (composeTALs fs gs)
