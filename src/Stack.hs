module Stack where

-- Stones Stack in each Cell. Nothing to do with Stack the tool

import Tak

-- turn all stones to F
flattenStack :: Stack -> Stack
flattenStack = map (\(Stone p _) -> Stone p F)

popStones :: XY -> Count -> Cell -> Cell
popStones xy count c@(Cell xy' zs)
  | xy == xy' = Cell xy (drop count zs)
  | otherwise = c

pushStones :: XY -> Stack -> Cell -> Cell
pushStones xy stones c@(Cell xy' zs)
  | xy == xy' = Cell xy (stones ++ flattenStack zs)
  | otherwise = c

