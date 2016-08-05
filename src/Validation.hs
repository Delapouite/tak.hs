module Validation where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (nub)

import Tak
import Board
import Cell
import Conversion

-- carry limit, stack height
isValidCount :: Game -> Move -> Bool
isValidCount g (count, xy, _, _) = let
  b = board g
  validStackHeight = case getCell b xy of
    Just c -> getHeight c >= count
    Nothing -> False
  in count <= size g && validStackHeight

isValidDrops :: Count -> Drops -> Bool
isValidDrops c d = c == sum d

-- only in empty cells
canPlace :: Board -> XY -> Bool
canPlace b xy = case getCell b xy of
  Just c -> isEmpty c
  Nothing -> False

capsInDeck :: Game -> Bool
capsInDeck g = let
  totalCaps = capCount $ size g
  placedCaps = length $ getPlacedByPlayerAndType (board g) (player g) C
  in totalCaps - placedCaps > 0

checkRoad :: Board -> (Cell -> Bool) -> [Cell] -> Cell -> Bool
checkRoad b isEnd visited c@(Cell xy _) = let
  newNeighbors = filter (`notElem` visited) $ getValidNeighbors b xy
  in isEnd c || any (checkRoad b isEnd (c:visited)) newNeighbors

checkHalfRoads :: Board -> (Cell -> Bool) -> [Cell] -> [Player]
checkHalfRoads b isEnd starts = let
  roadOwner c
    | checkRoad b isEnd starts c = getOwner c
    | otherwise                  = Nothing
  in nub $ mapMaybe roadOwner starts

checkRoads :: Game -> Maybe Display
checkRoads g
  | not $ null roadOwners = Just ("Road(s) completed by: " ++ show roadOwners)
  | otherwise             = Nothing
  where
    b = board g
    -- horizontal (West → East): col a → col maxX
    hStarts = getOwned $ getCol b 'a'
    hIsEnd (Cell (x, _) _) = x == xs !! (size g - 1)
    hRoadOwners = checkHalfRoads b hIsEnd hStarts
    -- vertical (South → North): row 1 → row maxY
    vStarts = getOwned $ getRow b 1
    vIsEnd (Cell (_, y) _) = y == size g
    vRoadOwners = checkHalfRoads b vIsEnd vStarts

    roadOwners = nub $ hRoadOwners ++ vRoadOwners

checkEnd :: Game -> Maybe Display
checkEnd g
  | isBoardFull b = Just ("Board's full! Flat winner is " ++ show winner)
  | otherwise     = checkRoads g
  where
    b = board g
    p1FlatsCount = length $ getPlacedByPlayerAndType b P1 F
    p2FlatsCount = length $ getPlacedByPlayerAndType b P2 F
    winner = if p1FlatsCount > p2FlatsCount then P1 else P2

isUnderControl :: Game -> XY -> Bool
isUnderControl g xy = case getCell (board g) xy of
  Nothing -> False
  Just c -> case getOwner c of
    Nothing -> False
    Just owner -> owner == player g

isDropzoneClear :: Game -> Move -> Bool
isDropzoneClear g (count, xy, dir, drops) = let
  b = board g
  Just cell = getCell b xy
  nextCells = catMaybes $ getNextCells b cell dir drops

  -- conditions
  inBounds = length nextCells == length drops
  initToppable = all isToppable $ init nextCells
  end = last nextCells
  isValidEnd = isToppable end || (hasCap cell && isFlattenable end drops)

  in inBounds && initToppable && isValidEnd

