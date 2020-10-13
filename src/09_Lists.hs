module Lists where


splitThem :: Char -> String -> [String]
splitThem _ [] = []
splitThem sep (x:xs)
  | x == sep = splitThem sep xs
splitThem sep str = let
  word = takeWhile (/= sep) str
  rest = dropWhile (/= sep) str
  in word:(splitThem sep rest)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y):(myZipWith f xs ys)

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWith (,)


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs 

myOr :: [Bool] -> Bool
myOr = myAny id

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (==x)

--TODO this will not work on infinite lists
--also we haven't studied folds yet but ok
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

squish :: [[a]] -> [a]
squish [] = []
squish (xs:yys) = case xs of
  []   -> squish yys
  a:as -> a:(squish (as:yys))

squish1 :: [[a]] -> [a]
squish1 = foldr (\xs ys-> foldr (:) ys xs) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . (map f)

(-.) :: (a -> b) -> (b -> c) -> (a -> c)
(-.) = flip (.)

-- changed type signature to make total
myMaxBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaxBy f xs = case xs of
  [] -> Nothing
  x:[] -> Just x
  x:y:xs -> 
    let
      max = if f x y == GT then x else y
      rest = myMaxBy f xs
      result = case rest of 
        Just(rst) -> case f max rst of
          GT -> Just max
          _  -> Just rst
        Nothing -> Just max
    in result