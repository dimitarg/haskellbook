{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Adts where

import Data.Char

data Price = Price Integer
  deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline Int
  deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir 13

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Plane _ _) = undefined -- icky
getManu (Car manu _) = manu

data Example = MakeExample deriving Show

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (x, _) = tooMany x

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany (x + y)

instance (Num a, Ord a) => TooMany (a, a) where
  tooMany (x, y) = (x + y) > 77

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node left y right) 
  | x == y      = Node left y right
  | x <  y      = Node (insert' x left) y right 
  | otherwise   = Node left y (insert' x right)

-- not required to preserve ordering
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

testTree' :: BinaryTree Int
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = (preorder left) ++ (x:(preorder right))

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z tree = foldr f z (preorder tree)

blah = foldTree (\x y -> x * y) 1 testTree'

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf sub@(s:ss) (x:xs)
  | s == x    = isSubseqOf ss xs
  | otherwise = isSubseqOf sub xs

capitaliseWord :: String -> String
capitaliseWord [] = []
capitaliseWord (' ':xs) = ' ':(capitaliseWord xs)
capitaliseWord (x:xs) = (toUpper x):xs

capitaliseWords :: String -> [(String, String)]
capitaliseWords txt = 
  let
    wds = words txt
    capitalise x = (x, capitaliseWord x)
  in map capitalise wds

capitaliseParagraph :: String -> String
capitaliseParagraph xs = 
  let
      result = foldr (\x acc ->
         if x == '.' then x:(capitaliseWord acc) else x:acc
         ) "" xs
  in capitaliseWord result


-- Hutton's razor

data Expr = Lit Integer | Add Expr Expr
  deriving Eq

eval :: Expr -> Integer
eval (Lit x) = x 
eval (Add l r) = (eval l) + (eval r)

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add l r) = (printExpr l) ++ " + " ++ (printExpr r)

