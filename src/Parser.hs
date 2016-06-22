module Parser where

import Data.Char (digitToInt, isAlpha, isDigit, toLower, toUpper)
import Data.List (intercalate, intersperse)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Tak
import Conversion

parseAction :: String -> Action
parseAction s = Action verb args
  where
    (verb:args) = words s

parseXY :: String -> Maybe XY
parseXY (x:y:[]) = if isAlpha x && isDigit y then Just (toLower x, read [y]) else Nothing
parseXY _ = Nothing

-- PTN: (stone)(square)
parsePlace :: String -> Maybe (StoneType, XY)
parsePlace (st:x:y:[]) = case readMaybe [toUpper st] of
  Just s -> case parseXY [x, y] of
    Just xy -> Just (s, xy)
    _ -> Nothing
  _ -> Nothing
parsePlace xy = case parseXY xy of
  Just xy -> Just (F, xy)
  _ -> Nothing

-- PTN: (count)(square)(direction)(drops count)(stone)
parseMove :: String -> Maybe Move
parseMove str = case parseCount str of
  (count, x:y:d:drops) -> case parseXY (x:[y]) of
    Nothing -> Nothing
    Just xy -> case parseDir d of
      Nothing -> Nothing
      Just dir -> Just (count, xy, dir, parseDrops count drops)
  -- not enough chars
  _ -> Nothing

-- default to 1
parseCount :: String -> (Count, String)
parseCount (h:str) = if isDigit h
  then (read [h], str)
  else (1, h:str)

parseDir :: Char -> Maybe Dir
parseDir c = case c of
  '<' -> Just West
  '>' -> Just East
  '+' -> Just North
  '-' -> Just South
  _ -> Nothing

parseDrops :: Count -> String -> Drops
parseDrops c str = case reads str :: [(Int, String)] of
  [(drops, _)] -> drops
  [] -> c

-- [TPS "x3,12,2S/x,22S,22C,11,21/121,212,12,1121C,1212S/21S,1,21,211S,12S/x,21S,2,x2 1 26"]
parseTPSBoard :: String -> Board
parseTPSBoard tps = concat [parseTPSRow r y | (r, y) <- z]
  where
    tpsRows = splitOn "/" tps
    z = zip tpsRows $ reverse [1..(length tpsRows)]

parseTPSRow :: String -> Y -> Row
parseTPSRow tps y = cells
  where
    tpsRow = intercalate "," . map parseTPSX $ splitOn "," tps
    tpsCell =  splitOn "," tpsRow
    cells = [parseTPSCell c (x, y) | (c, x) <- zip tpsCell $ take (length tpsCell) xs]

parseTPSX :: String -> String
parseTPSX ['x', n] = intersperse ',' $ replicate (digitToInt n) 'x'
parseTPSX tps = tps

parseTPSCell :: String -> XY -> Cell
parseTPSCell tps xy = Cell xy (parseTPSStack tps)

parseTPSStack :: String -> Stack
parseTPSStack "x" = []
parseTPSStack tps = stones'
  where
    lastSType = if last tps `elem` "SC" then read [last tps] :: StoneType else F
    chars = if lastSType /= F then init tps else tps
    stones = map (\c -> Stone (toPlayer [c]) F) chars
    stones' = init stones ++ map (\(Stone p _) -> Stone p lastSType) [last stones]
