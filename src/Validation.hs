module Validation where

import Data.Maybe (mapMaybe)
import Data.List (nub)

import Tak
import Board
import Cell

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

checkRoads :: Board -> Maybe Display
checkRoads b
  | not $ null roadOwners = Just ("Road(s) completed by: " ++ show roadOwners)
  | otherwise             = Nothing
  where
    s = getSize b
    -- horizontal (West → East): col a → col maxX
    hStarts = getOwned $ getCol b 'a'
    hIsEnd (Cell (x, _) _) = x == xs !! (s - 1)
    hRoadOwners = checkHalfRoads b hIsEnd hStarts
    -- vertical (South → North): row 1 → row maxY
    vStarts = getOwned $ getRow b 1
    vIsEnd (Cell (_, y) _) = y == s
    vRoadOwners = checkHalfRoads b vIsEnd vStarts

    roadOwners = nub $ hRoadOwners ++ vRoadOwners

checkEnd :: Game -> Maybe Display
checkEnd Game {board = b}
  | isBoardFull b = Just ("Board's full! Flat winner is " ++ show winner)
  | otherwise     = checkRoads b
  where
    p1FlatsCount = length $ getPlacedByPlayerAndType b P1 F
    p2FlatsCount = length $ getPlacedByPlayerAndType b P2 F
    winner = if p1FlatsCount > p2FlatsCount then P1 else P2

