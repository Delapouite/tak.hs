module Validation where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe, isJust, fromJust)
import Data.List (nub)

import Tak
import Cell
import Conversion

-- coordinates

isValidX :: Game -> X -> Bool
isValidX g x = x' >= 0 && x' <= size g
  where
    x' = xToInt x

isValidY :: Game -> Y -> Bool
isValidY g y = y > 0 && y <= size g

-- in bounds
isValidXY :: Game -> XY -> Bool
isValidXY g (x,y) = isValidX g x && isValidY g y

isValidSize :: Int -> Bool
isValidSize s = s >= minSize && s <= maxSize

-- carry limit, stack height
isValidCount :: Game -> Move -> Bool
isValidCount g (count, xy, _, _) =
  count <= size g && validStackHeight
  where
    b = board g
    validStackHeight = case getCell b xy of
      Just c -> getHeight c >= count
      Nothing -> False

isValidDrops :: Count -> Drops -> Bool
isValidDrops c d = c == d || c == (sum . map digitToInt . show) d

getOwned :: [Cell] -> [Cell]
getOwned = filter (not . isEmpty)

-- only in empty cells
canPlace :: Board -> XY -> Bool
canPlace b xy = case getCell b xy of
  Just c -> isEmpty c
  Nothing -> False

capsInDeck :: Game -> Bool
capsInDeck g = totalCaps - placedCaps > 0
  where
    totalCaps = capCount $ size g
    placedCaps = length $ getPlacedByPlayerAndType (board g) (player g) C

isBoardFull :: Board -> Bool
isBoardFull = not . any isEmpty

checkRoad :: Board -> (Cell -> Bool) -> [Cell] -> Cell -> Bool
checkRoad b isEnd visited c@(Cell xy _) =
  isEnd c || any (checkRoad b isEnd (c:visited)) newNeighbors
  where
    newNeighbors = filter (`notElem` visited) $ getValidNeighbors b xy

checkHalfRoads :: Board -> (Cell -> Bool) -> [Cell] -> [Player]
checkHalfRoads b isEnd starts = nub $ mapMaybe roadOwner starts
  where
    roadOwner c = if checkRoad b isEnd starts c
      then getOwner c
      else Nothing

checkRoads :: Game -> Maybe Display
checkRoads g = if not $ null roadOwners
  then Just ("Road(s) completed by: " ++ show roadOwners)
  else Nothing
  where
    b = board g
    -- horizontal (West → East): col a → col maxX
    hStarts = getOwned $ getCol b 'a'
    hIsEnd (Cell (x, _) _) = x == getMaxX b
    hRoadOwners = checkHalfRoads b hIsEnd hStarts
    -- vertical (South → North): row 1 → row maxY
    vStarts = getOwned $ getRow b 1
    vIsEnd (Cell (_, y) _) = y == size g
    vRoadOwners = checkHalfRoads b vIsEnd vStarts

    roadOwners = nub $ hRoadOwners ++ vRoadOwners

checkEnd :: Game -> Maybe Display
checkEnd g = if isBoardFull b
  then Just ("Board's full! Flat winner is " ++ show winner)
  else checkRoads g
  where
    b = board g
    p1FlatsCount = length $ getPlacedByPlayerAndType b P1 F
    p2FlatsCount = length $ getPlacedByPlayerAndType b P2 F
    winner = if p1FlatsCount > p2FlatsCount then P1 else P2

isUnderControl :: Game -> Move -> Bool
isUnderControl g (_, xy, _, _) = case getCell (board g) xy of
  Nothing -> False
  Just c -> case getTopStone c of
    Nothing -> False
    Just (Stone owner _) -> owner == player g

isDropzoneClear :: Game -> Move -> Bool
isDropzoneClear g (count, xy, dir, drops) = clear
  where
    b = board g
    Just cell = getCell b xy
    nextMCells = filter isJust $ getNextCells b cell dir drops
    nextCells = map fromJust nextMCells

    -- conditions
    inBounds = length nextMCells == (length . show) drops
    initToppable = all isToppable $ init nextCells
    end = last nextCells
    isValidEnd = isToppable end || (hasCap cell && isFlattenable end drops)

    clear = inBounds && initToppable && isValidEnd

