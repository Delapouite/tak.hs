{-# LANGUAGE TupleSections #-}

module Parser where

import Data.Char (digitToInt, isAlpha, isDigit, toLower, toUpper)
import Data.List (intercalate, intersperse)
import Data.List.Split (splitOn)
import Data.Maybe
import Text.Read (readMaybe)

import Tak

parseCommand :: String -> Command
-- default
parseCommand "" = Command "show" []
parseCommand s = let
  (verb:args) = words s
  in Command verb args

parseXY :: String -> Maybe XY
parseXY [x, y]
  | isAlpha x && isDigit y = Just (toLower x, read [y])
  | otherwise              = Nothing
parseXY _ = Nothing

-- PTN: (stone)(square)
parsePlace :: String -> Maybe (StoneType, XY)
parsePlace [st, x, y] = do
  s <- readMaybe [toUpper st]
  (s,) <$> parseXY [x, y]
parsePlace [x, y] = (F,) <$> parseXY [x, y]
parsePlace _ = Nothing

-- PTN: (count)(square)(direction)(drops count)(stone)
parseMove :: String -> Maybe Move
parseMove str = case parseCount str of
  (count, x:y:d:drops) -> do
    xy <- parseXY (x:[y])
    (count, xy, , parseDrops count drops) <$> parseDir d
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
