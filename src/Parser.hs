module Parser where

import Data.Char (isAlpha, isDigit, toLower, toUpper)
import Text.Read (readMaybe)

import Tak

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
      Just dir -> Just (count, xy, dir, (parseDrops count drops))
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

