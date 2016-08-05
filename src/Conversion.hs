module Conversion where

import Data.Char (isDigit, ord, toUpper)

import Tak

-- 97 = ASCII 'a'
xToInt :: X -> Int
xToInt x = ord x - 97

toXorY :: String -> Either X Y
toXorY (h:_)
  | isDigit h = Right (read [h] :: Int)
  | otherwise = Left h

toStoneType :: String -> StoneType
toStoneType str = read (map toUpper str) :: StoneType

toPlayer :: String -> Player
toPlayer "1" = P1
toPlayer _ = P2
