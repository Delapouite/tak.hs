module Conversion where

import Data.Char (isDigit, ord, toUpper)

import Tak

-- 97 = ASCII 'a'
xToInt :: X -> Int
xToInt x = ord x - 97

toXorY :: String -> Either X Y
toXorY arg
  | isDigit $ head arg = Right (read [head arg] :: Int)
  | otherwise          = Left (head arg)

toStoneType :: String -> StoneType
toStoneType str = read (map toUpper str) :: StoneType

toPlayer :: String -> Player
toPlayer "1" = P1
toPlayer _ = P2
