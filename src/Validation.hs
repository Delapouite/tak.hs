module Validation where

import Data.Char (digitToInt)
import Data.Maybe (isJust, fromJust)

import Tak
import Conversion

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

-- only in empty cells
canPlace :: Board -> XY -> Bool
canPlace b xy = case getCell b xy of
  Just (Cell _ zs) -> null zs
  Nothing -> False

capsInDeck :: Game -> Bool
capsInDeck g = totalCaps - placedCaps > 0
  where
    p = getPlayer g
    totalCaps = capCount $ size g
    placedCaps = length $ getPlacedByPlayerAndType g p C

isBoardFull :: Board -> Bool
isBoardFull = not . any (\(Cell _ zs) -> null zs)

checkEnd :: Game -> Maybe Display
checkEnd g = if isBoardFull $ board g
  then Just ("Board's full! Flat winner is " ++ show winner)
  else Nothing
  where
    p1FlatsCount = length $ getPlacedByPlayerAndType g P1 F
    p2FlatsCount = length $ getPlacedByPlayerAndType g P2 F
    winner = if p1FlatsCount > p2FlatsCount then P1 else P2

isUnderControl :: Game -> Move -> Bool
isUnderControl g (_, xy, _, _) = case getCell (board g) xy of
  Nothing -> False
  Just c -> case getTopStone c of
    Nothing -> False
    Just (Stone owner _) -> owner == getPlayer g

-- F or empty cell
isToppable :: Cell -> Bool
isToppable c = case getTopStone c of
  Just (Stone _ t) -> t == F
  Nothing -> True

-- C on top of stack
hasCap :: Cell -> Bool
hasCap c = case getTopStone c of
  Just (Stone _ t) -> t == C
  Nothing -> False

-- S and last drop == 1
isFlattenable :: Cell -> Drops -> Bool
isFlattenable c drops = case getTopStone c of
  Nothing -> False
  -- a cap can only flatten when alone
  Just (Stone _ t) -> t == S && ((last . show $ drops) == '1')

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

