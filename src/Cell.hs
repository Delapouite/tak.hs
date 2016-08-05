module Cell where

import Tak

getHeight :: Cell -> Int
getHeight (Cell _ zs) = length zs

getOwner :: Cell -> Maybe Player
getOwner c = case getTopStone c of
  Nothing -> Nothing
  Just (Stone owner _) -> Just owner

getTopStone :: Cell -> Maybe Stone
getTopStone (Cell _ []) = Nothing
getTopStone (Cell _ (s:_)) = Just s

-- C on top of stack
hasCap :: Cell -> Bool
hasCap c = case getTopStone c of
  Just (Stone _ t) -> t == C
  Nothing -> False

isEmpty :: Cell -> Bool
isEmpty (Cell _ zs) = null zs

-- F or empty cell
isToppable :: Cell -> Bool
isToppable c = case getTopStone c of
  Just (Stone _ t) -> t == F
  Nothing -> True

-- S and last drop == 1
isFlattenable :: Cell -> Drops -> Bool
isFlattenable c drops = case getTopStone c of
  Nothing -> False
  -- a cap can only flatten when alone
  Just (Stone _ t) -> t == S && ((last . show $ drops) == '1')

