module Board where

import Data.List (find, sortBy, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)

import Tak
import Cell
import Conversion
import Stack
import XY

initBoard :: Size -> Board
initBoard size = take (size ^ 2) [Cell (x, y) [] | x <- xs, y <- [1..size]]

sortBoard :: Board -> Board
sortBoard = let
  sorter (Cell (x, y) _) (Cell (x', y') _)
    | x < x' = LT
    | x > x' = GT
    | x == x' = compare y y'
  in sortBy sorter

getCell :: Board -> XY -> Maybe Cell
getCell b xy = find (\(Cell xy' _) -> xy == xy') b

getCol :: Board -> X -> Col
getCol b x = toCols b !! xToInt x

getMaxHeight :: [Cell] -> Int
getMaxHeight = maximum . map getHeight

-- up to 4, in each direction
getNeighbors :: Board -> XY -> [Cell]
getNeighbors b xy = let
  xys = mapMaybe (getNextXY (getSize b) xy) [North, East, South, West]
  in mapMaybe (getCell b) xys

getNextCells :: Board -> Cell -> Dir -> Drops -> [Cell]
getNextCells b (Cell xy _) dir drops =
  mapMaybe (getCell b) $ getNextXYs (getSize b) xy dir drops

getNextXY :: Size -> XY -> Dir -> Maybe XY
getNextXY s (x, y) d = let
  xy = case d of
    North -> (x, succ y)
    East  -> (succ x, y)
    South -> (x, pred y)
    West  -> (pred x, y)
  in if isValidXY s xy then Just xy else Nothing

getNextXYs :: Size -> XY -> Dir -> Drops -> [XY]
getNextXYs s xy dir [d] = case getNextXY s xy dir of
  Just xy' -> [xy']
  Nothing -> []
getNextXYs s xy dir (d:ds) = let
  [xy'] = getNextXYs s xy dir [d]
  in xy' : getNextXYs s xy' dir ds

getOwned :: [Cell] -> [Cell]
getOwned = filter (not . isEmpty)

getPlacedByPlayer :: Board -> Player -> [Stone]
getPlacedByPlayer b p = let
  getOwnStones = filter (\(Stone owner _) -> owner == p)
  in concatMap getOwnStones $ getStacks b

getPlacedByPlayerAndType :: Board -> Player -> StoneType -> [Stone]
getPlacedByPlayerAndType b p st = filter (\(Stone _ t) -> t == st) $ getPlacedByPlayer b p

getRow :: Board -> Y -> Row
getRow b y = toRows b !! (y - 1)

getSize :: Board -> Size
getSize = truncate . sqrt . fromIntegral . length

getStacks :: Board -> [Stack]
getStacks = map (\(Cell _ zs) -> zs)

-- neighbors owned by same player
getValidNeighbors :: Board -> XY -> [Cell]
getValidNeighbors b xy = case getCell b xy of
  Nothing -> []
  Just cell -> case getOwner cell of
    Nothing -> []
    Just owner -> filter (isOwnedBy owner) $ getNeighbors b xy

isBoardFull :: Board -> Bool
isBoardFull = not . any isEmpty

toCols :: Board -> [Col]
toCols b = chunksOf (getSize b) b

toRows :: Board -> [Row]
toRows = transpose . toCols

-- stack

placeStone :: Board -> XY -> Player -> StoneType -> Board
placeStone b xy p st = map (pushStones xy [Stone p st]) b

moveSubstack :: Board -> Count -> XY -> XY -> Board
moveSubstack b count fromXY toXY = let
  Just (Cell _ zs) = getCell b fromXY
  stones = take count zs
  b' = map (popStones fromXY count) b
  in map (pushStones toXY stones) b'

moveStack :: Board -> Move -> Board
moveStack b m@(_, xy, dir, _) = let
  size = getSize b
  reducer acc (xy, drop) = case getNextXY size xy dir of
    Just xy' -> moveSubstack acc drop xy xy'
    Nothing -> acc
  in foldl reducer b $ zipXYandCounts size m

zipXYandCounts :: Size -> Move -> [(XY, Count)]
zipXYandCounts s (count, xy, dir, drops) = let
  reducer acc drop = acc ++ [last acc - drop]
  xys = init $ xy : getNextXYs s xy dir drops
  counts = foldl reducer [count] $ init drops
  in zip xys counts
