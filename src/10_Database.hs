module Database where

import Data.Time

data DatabaseItem = 
  DbString String | DbNumber Integer | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
  (fromGregorian 1911 5 1)
  (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
  (fromGregorian 1921 5 1)
  (secondsToDiffTime 34123))
  ]

myFilter :: (a -> Maybe b) -> [a] -> [b]
myFilter f = foldr (\a acc ->
    case f a of
      Just b -> b:acc
      Nothing -> acc
  ) []

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = myFilter ( \x -> case x of
  DbDate d -> Just d
  _        -> Nothing
  )

filterDbNum :: [DatabaseItem] -> [Integer]
filterDbNum = let
  f x = case x of
    DbNumber n -> Just n
    _          -> Nothing
  in myFilter f

mostRecent :: [DatabaseItem] -> Maybe UTCTime
mostRecent xs = let
  f a acc = if (Just a) > acc then Just a else acc
  in foldr f Nothing (filterDbDate xs)

sumDb :: [DatabaseItem] -> Integer
sumDb xs = 
  let
    nums = filterDbNum xs
    result = foldr (+) 0 nums
  in result

avgDb :: [DatabaseItem] -> Double
avgDb xs =
  let
    summed = sumDb xs
    result = (fromIntegral summed) / (fromIntegral ((length . filterDbNum) xs))
  in result