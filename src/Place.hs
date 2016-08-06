module Place where

-- validators for Place command

import Data.Maybe

import Tak
import Board
import Cell

-- only in empty cells
canPlace :: Board -> XY -> Bool
canPlace b xy = maybe False isEmpty $ getCell b xy

-- player still has caps to play?
capsInDeck :: Board -> Player -> Bool
capsInDeck b p = let
  totalCaps = capCount $ getSize b
  placedCaps = length $ getPlacedByPlayerAndType b p C
  in totalCaps - placedCaps > 0
