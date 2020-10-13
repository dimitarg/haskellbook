module Cipher where
import Data.Char

ceasarRight :: Int -> String -> String
ceasarRight cnt str = let
  ceasarRightC :: Int -> Char -> Char
  ceasarRightC cnt char = let
    actualCnt = mod cnt 26
    limitChr = case isUpper char of
      True  -> 'Z'
      False -> 'z'
    limit = ord limitChr
    result = actualCnt + ord char
    normalised = case result > limit of
      True -> result - 26
      False -> result
    in chr normalised
  in fmap (ceasarRightC cnt) str

unceasarRight :: Int -> String -> String
unceasarRight cnt = ceasarRight (-cnt)
