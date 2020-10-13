{-# LANGUAGE LambdaCase #-}
import Control.Monad (guard)
stops = "pbtdkg"
vowels = "aeiou"

svs :: [(Char, Char, Char)]
svs = do
  x <- stops
  y <- vowels
  z <- stops
  return (x, y, z)

svsp :: [(Char, Char, Char)]
svsp = filter (\ (x, _, _) -> x == 'p') svs

nouns = ["table", "dude", "shrubbery"]
verbs = ["eat", "watch", "bother"]

nvn = do
  n1 <- nouns
  v <- verbs
  n2 <- nouns
  -- just for kicks
  guard (n1 /= n2)
  return (n1, v, n2)

seekritFunc x = --div (sum (map length (words x))) (length (words x))
  let
    numWords = (length . words) x
    sumLengths = sum (map length (words x))
  in sumLengths `div` numWords

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
-- \a acc -> f a || acc
myAny f = foldr ( (||) . f ) False 

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (x == )

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold x = foldr (\a acc -> x == a || acc) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
-- myMap f = foldr (\a acc -> (f a):acc) []
myMap f = foldr ((:).f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a acc -> if f a then a:acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
-- (\a acc -> f a ++ acc)
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = 
  let 
    bigger x y = case (f x y) of 
      LT -> y
      _  -> x
  in foldl bigger (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = 
  let 
    smaller x y = case (f x y) of 
      GT -> y
      _  -> x
  in foldl smaller (head xs) xs