module Cell where

import Tak

-- associated player

getOwner :: Cell -> Maybe Player
getOwner = (\(Stone p _) -> p) <$> getTopStone

isOwnedBy :: Player -> Cell -> Bool
isOwnedBy p c = case getOwner c of
  Just owner -> owner == p
  Nothing -> False

-- associated stack
--
getHeight :: Cell -> Int
getHeight (Cell _ zs) = length zs

getTopStone :: Cell -> Maybe Stone
getTopStone (Cell _ []) = Nothing
getTopStone (Cell _ (s:_)) = Just s

-- no stones
isEmpty :: Cell -> Bool
isEmpty (Cell _ zs) = null zs

-- C on top of stack
isCapped :: Cell -> Bool
isCapped c = case getTopStone c of
  Just (Stone _ t) -> t == C
  Nothing -> False

-- F or empty cell
isToppable :: Cell -> Bool
isToppable c = case getTopStone c of
  Just (Stone _ t) -> t == F
  Nothing -> True

-- S and last drop == 1
isFlattenable :: Cell -> Drops -> Bool
isFlattenable c drops = case getTopStone c of
  -- a cap can only flatten when alone
  Just (Stone _ t) -> t == S && (last drops == 1)
  Nothing -> False

