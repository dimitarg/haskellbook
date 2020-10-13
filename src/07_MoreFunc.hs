module MoreFunc where

myVal :: Num p1 => p2 -> p1
myVal _ = 42

addOneIfOdd :: Integral p => p -> p
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \a -> a + 1

addFive :: Integer -> Integer -> Integer
addFive = \x y -> (if x > y then y else x) + 5

mflip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
mflip f x y = f y x

zzzz :: Int -> Bool
zzzz 2 = True
zzzz 3 = True
zzzz _ = False

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

functionC :: Ord a => a -> a -> a
functionC x y = case x > y of
  True -> x
  False -> y

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0