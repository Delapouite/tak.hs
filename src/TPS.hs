module TPS where

import Data.Char (digitToInt, isAlpha, isDigit, toLower, toUpper)
import Data.List (intercalate, intersperse)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

import Tak
import Conversion

-- [TPS "x3,12,2S/x,22S,22C,11,21/121,212,12,1121C,1212S/21S,1,21,211S,12S/x,21S,2,x2 1 26"]
parseTPS :: String -> Game
parseTPS tps = Game { size = getSize b', board = b', player = p', turn = read t }
  where
    [_, g, _] = splitOn "\"" tps
    [b, p, t] = splitOn " " g
    b' = parseTPSBoard b
    p' = toPlayer p

parseTPSBoard :: String -> Board
parseTPSBoard tps = concat [parseTPSRow r y | (r, y) <- z]
  where
    tpsRows = splitOn "/" tps
    z = zip tpsRows $ reverse [1..(length tpsRows)]

-- x5
-- x3,12,2S
parseTPSRow :: String -> Y -> Row
parseTPSRow tps y = cells
  where
    tpsRow = intercalate "," . map parseTPSX $ splitOn "," tps
    tpsCell =  splitOn "," tpsRow
    cells = [parseTPSCell c (x, y) | (c, x) <- zip tpsCell $ take (length tpsCell) xs]

-- x5 → x,x,x,x,x
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

