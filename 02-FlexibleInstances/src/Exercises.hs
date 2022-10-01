module Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

-- instance PopQuiz Bool
-- no
-- instance PopQuiz [Bool]
-- yes
-- instance PopQuiz [a]
-- no
-- instance PopQuiz (a, b)
-- no
-- instance PopQuiz [(a, b)]
-- yes
-- instance PopQuiz (IO a)
-- no

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/.
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a)
-- yes
-- instance PopQuiz (RIO r a)
-- no
-- instance PopQuiz (RIO' r a)
-- yes
-- instance PopQuiz (r -> IO a)
-- yes
-- instance PopQuiz (a -> b) -- We can write (a -> b) as ((->) a b).
-- no
-- instance PopQuiz (a -> b -> c)
-- yes
-- instance PopQuiz (a, b, c)
-- no
-- instance PopQuiz (a, (b, c))
-- yes
-- instance PopQuiz ()
-- no
-- instance PopQuiz (a, b, c, a)
-- yes

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a)
-- yes
-- instance PopQuiz (Pair a)
-- no
-- instance PopQuiz (Pair' a)
-- yes
