module Place where

import Data.Maybe

import Tak
import Board
import Cell
import Stack

-- validators

-- only in empty cells
canPlace :: Board -> XY -> Bool
canPlace b xy = maybe False isEmpty $ getCell b xy

-- player still has caps to play?
capsInDeck :: Board -> Player -> Bool
capsInDeck b p = let
  totalCaps = capCount $ getSize b
  placedCaps = length $ getPlacedByPlayerAndType b p C
  in totalCaps - placedCaps > 0

-- data update

placeStone :: Board -> XY -> Player -> StoneType -> Board
placeStone b xy p st = map (pushStones xy [Stone p st]) b
