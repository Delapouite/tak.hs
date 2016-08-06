module Move where

-- validators for Move command

import Tak
import Board
import Cell

-- carry limit, stack height
isValidCount :: Board -> Move -> Bool
isValidCount b (count, xy, _, _) = let
  validStackHeight = case getCell b xy of
    Just c -> getHeight c >= count
    Nothing -> False
  in count <= getSize b && validStackHeight

isValidDrops :: Count -> Drops -> Bool
isValidDrops c d = c == sum d

-- only move stacks owned by current p
isValidOwner :: Board -> Player -> XY -> Bool
isValidOwner b p xy = case getCell b xy of
  Nothing -> False
  Just c -> case getOwner c of
    Nothing -> False
    Just owner -> owner == p

isDropzoneClear :: Board -> Move -> Bool
isDropzoneClear b (count, xy, dir, drops) = case getCell b xy of
  Nothing -> False
  Just cell -> let
    nextCells = getNextCells b cell dir drops
    end = last nextCells

    -- conditions
    inBounds = length nextCells == length drops
    initToppable = all isToppable $ init nextCells
    isValidEnd = isToppable end || (hasCap cell && isFlattenable end drops)

    in inBounds && initToppable && isValidEnd

