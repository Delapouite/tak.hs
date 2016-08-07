module Cell where

import Data.Maybe

import Tak

-- associated player

getOwner :: Cell -> Maybe Player
getOwner c = (\(Stone p _) -> p) <$> getTopStone c

isOwnedBy :: Player -> Cell -> Bool
isOwnedBy p c = maybe False (== p) $ getOwner c

-- associated stack
--
getHeight :: Cell -> Int
getHeight (Cell _ zs) = length zs

getTopStone :: Cell -> Maybe Stone
getTopStone (Cell _ zs) = listToMaybe zs

-- no stones
isEmpty :: Cell -> Bool
isEmpty (Cell _ zs) = null zs

-- C on top of stack
isCapped :: Cell -> Bool
isCapped c = maybe False (\(Stone _ t) -> t == C) $ getTopStone c

-- F or empty cell
isToppable :: Cell -> Bool
isToppable c = maybe True (\(Stone _ t) -> t == F) $ getTopStone c

-- S and last drop == 1
-- a cap can only flatten when alone
isFlattenable :: Cell -> Drops -> Bool
isFlattenable c drops =
  maybe False (\(Stone _ t) -> t == S && (last drops == 1)) $ getTopStone c

