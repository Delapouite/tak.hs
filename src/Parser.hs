module Parser where

import Data.Char (digitToInt, isAlpha, isDigit, toLower, toUpper)
import Data.List (intercalate, intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

import Tak

parseCommand :: String -> Command
-- default
parseCommand "" = Command "show" []
parseCommand s = let
  (verb:args) = words s
  in Command verb args

parseXY :: String -> Maybe XY
parseXY (x:y:[])
  | isAlpha x && isDigit y = Just (toLower x, read [y])
  | otherwise              = Nothing
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
parseCount (h:str)
  | isDigit h = (read [h], str)
  | otherwise = (1, h:str)

parseDir :: Char -> Maybe Dir
parseDir c = case c of
  '<' -> Just West
  '>' -> Just East
  '+' -> Just North
  '-' -> Just South
  _ -> Nothing

parseDrops :: Count -> String -> Drops
parseDrops c str = case reads str :: [(Int, String)] of
  [(drops, _)] -> map digitToInt $ show drops
  [] -> [c]

parseUnknownVerb :: Verb -> Maybe Command
parseUnknownVerb v
  | isJust $ parsePlace v = Just $ Command "place" [v]
  | isJust $ parseMove v = Just $ Command "move" [v]
  | otherwise = Nothing
