module Move where

import Data.Maybe

import Tak
import Board
import Cell
import Stack

-- validators

-- carry limit, stack height
isValidCount :: Board -> Move -> Bool
isValidCount b (count, xy, _, _) = let
  validStackHeight = maybe False (\c -> getHeight c >= count) $ getCell b xy
  in count <= getSize b && validStackHeight

isValidDrops :: Count -> Drops -> Bool
isValidDrops c d = c == sum d

-- only move stacks owned by current p
isValidOwner :: Board -> Player -> XY -> Bool
isValidOwner b p xy = maybe False (isOwnedBy p) $ getCell b xy

isDropzoneClear :: Board -> Move -> Bool
isDropzoneClear b (count, xy, dir, drops) = case getCell b xy of
  Nothing -> False
  Just cell -> let
    nextCells = getNextCells b cell dir drops
    end = last nextCells

    -- conditions
    inBounds = length nextCells == length drops
    initToppable = all isToppable $ init nextCells
    isValidEnd = isToppable end || (isCapped cell && isFlattenable end drops)

    in inBounds && initToppable && isValidEnd

-- data updates

moveSubstack :: Board -> Count -> XY -> XY -> Board
moveSubstack b count fromXY toXY = let
  Just (Cell _ zs) = getCell b fromXY
  stones = take count zs
  b' = map (popStones fromXY count) b
  in map (pushStones toXY stones) b'

moveStack :: Board -> Move -> Board
moveStack b m@(_, xy, dir, _) = let
  size = getSize b
  reducer acc (xy, drop) = case getNextXY size xy dir of
    Just xy' -> moveSubstack acc drop xy xy'
    Nothing -> acc
  in foldl reducer b $ zipXYandCounts size m

zipXYandCounts :: Size -> Move -> [(XY, Count)]
zipXYandCounts s (count, xy, dir, drops) = let
  reducer acc drop = acc ++ [last acc - drop]
  xys = init $ xy : getNextXYs s xy dir drops
  counts = foldl reducer [count] $ init drops
  in zip xys counts
