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
isUnderControl :: Board -> Player -> XY -> Bool
isUnderControl b p xy = case getCell b xy of
  Nothing -> False
  Just c -> case getOwner c of
    Nothing -> False
    Just owner -> owner == p

isDropzoneClear :: Board -> Move -> Bool
isDropzoneClear b (count, xy, dir, drops) = let
  Just cell = getCell b xy
  nextCells = getNextCells b cell dir drops

  -- conditions
  inBounds = length nextCells == length drops
  initToppable = all isToppable $ init nextCells
  end = last nextCells
  isValidEnd = isToppable end || (hasCap cell && isFlattenable end drops)

  in inBounds && initToppable && isValidEnd

