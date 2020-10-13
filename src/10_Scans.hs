module Scans where

fibs :: [Int]
fibs = 1:(scanl (+) 1 fibs)

-- scanl + 1 [1..3]
-- [1, 1 + 1, 1 + 1 + 2, 1 + 1 + 2 + 3]

-- 1:(scanl (+) 1 fibs)
-- 1:([1, 1+1, ..])
-- 1:([1, 1+1, 1+2, ..])
-- 1:([1, 1+1, 1+2, 1+1 + 1+2, ...])



fibs20 = take 20 fibs

fibsLess = takeWhile (<100) fibs

--[1, 1, 2,]
myFact :: [Int]
myFact = scanl (*) 1 [1..]

myFactN n = myFact !! n