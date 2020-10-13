module Adversity where

import Data.Maybe

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe  x    = Just x

replaceThe :: String -> String
replaceThe text = 
  let
    wds = words text
    result = map (\x ->
        maybe "a" id (notThe x)
      ) wds
  in unwords result

isVowel :: Char -> Bool
isVowel x
  | x == 'a' || x == 'A' = True
  | x == 'e' || x == 'E' = True
  | x == 'i' || x == 'I' = True
  | x == 'o' || x == 'O' = True
  | x == 'u' || x == 'U' = True
  | otherwise            = False



countTheBeforeVowel :: String -> Integer
countTheBeforeVowel =
  let
    cnt :: [String] -> Integer
    cnt [] = 0
    cnt (_:[]) = 0
    cnt ("the":y:ys) = 
      let
        first = listToMaybe y
        isV = maybe False isVowel first
        result = if isV then 1 else 0
      in result + (cnt ys)
    cnt (_:xs) = cnt xs
      
  in cnt . words

vowels :: String -> String
vowels = filter isVowel

countVowels :: String -> Integer
countVowels = toInteger . length . vowels

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord wd = 
  let
    v = countVowels wd
    len =   toInteger (length wd)
    conso = len - v
    valid = conso >= v
  in 
    if valid then Just (Word' wd) else Nothing

data Nat = Zero| Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + (natToInteger x)

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x
  | x < 0     = Nothing
  | otherwise = fmap Succ (integerToNat (x-1))


-- maybee

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' _ f (Just x) = f x
maybe' z _ Nothing  = z


isJust' :: Maybe a -> Bool
isJust' = maybe' False (const True)

isNothing' :: Maybe a -> Bool
isNothing' = not . isJust'

fromMaybe' :: a -> Maybe a -> a
fromMaybe' x = maybe' x id

listToMaybe' :: [a] -> Maybe a
listToMaybe' (x:_)  = Just x
listToMaybe' _      = Nothing

maybeToList' :: Maybe a -> [a]
maybeToList' = maybe' [] (:[])

catMaybes' :: [Maybe a] -> [a]
catMaybes' xs = do
  x <- xs
  y <- maybeToList' x
  return y

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = 
  let 
    f :: Maybe a -> Maybe [a] -> Maybe [a]
    f (Just x) (Just xs) = Just (x:xs)
    f _ _ = Nothing
  in foldr f (Just [])

-- eeeitheer

asLeft :: Either a b -> Maybe a
asLeft (Left x) = Just x
asLeft _        = Nothing

flipEither :: Either a b -> Either b a
flipEither (Left x)  = Right x
flipEither (Right x) = Left x

asRight :: Either a b -> Maybe b
asRight = asLeft . flipEither

eitherToJusts :: (Either a b -> Maybe c) ->  [Either a b] -> [c]
eitherToJusts f = foldr (\x acc -> case f x of
    Just a  -> a:acc
    Nothing -> acc
  ) []

lefts' :: [Either a b] -> [a]
lefts' = eitherToJusts asLeft


rights' :: [Either a b] -> [b]
rights' = eitherToJusts asRight

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\x (as, bs) -> case x of 
    Left(a)  -> (a:as, bs)
    Right(b) -> (as, b:bs)
  ) ([], [])

eitherMaybe' :: (b -> c)
  -> Either a b
  -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just (f b)

--cata
either' ::
     (a -> c)
  -> (b -> c)
  -> Either a b
  -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c)
 -> Either a b
 -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f) -- yay pointless :)

-- anamorphisms
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a:(myIterate f (f a))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing -> []
  Just (a, b1) -> a:(myUnfoldr f b1)

betterIterate ::  (a -> a) -> a -> [a]
betterIterate f  a = myUnfoldr (\a -> Just (a, f a)) a

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold ::
    (a -> Maybe (a,b,a))
  -> a
  -> BinaryTree b

unfold f a = case f a of
  Just (l, b, r) -> Node (unfold f l) b (unfold f r)
  Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\i -> if i >= n then Nothing else Just(i+1, i, i+1)) 0