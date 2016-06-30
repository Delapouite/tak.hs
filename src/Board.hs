module Board where

import Data.List (find, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, isJust)

import Tak
import Cell
import Conversion

getCell :: Board -> XY -> Maybe Cell
getCell b xy = find (\(Cell xy' _) -> xy == xy') b

getCol :: Board -> X -> Col
getCol b x = toCols b !! xToInt x

getMaxX :: Board -> X
getMaxX b = xs !! (getSize b - 1)

getRow :: Board -> Y -> Row
getRow b y = toRows b !! (y - 1)

getSize :: Board -> Int
getSize = truncate . sqrt . fromIntegral . length

getStacks :: Board -> [Stack]
getStacks = map (\(Cell _ zs) -> zs)

toCols :: Board -> [Col]
toCols b = chunksOf (getSize b) b

toRows :: Board -> [Row]
toRows = transpose . toCols

getPlacedByPlayer :: Board -> Player -> [Stone]
getPlacedByPlayer b p = concatMap getOwnStones $ getStacks b
  where
    getOwnStones = filter (\(Stone owner _) -> owner == p)

getPlacedByPlayerAndType :: Board -> Player -> StoneType -> [Stone]
getPlacedByPlayerAndType b p st = filter (\(Stone _ t) -> t == st) $ getPlacedByPlayer b p

getMaxHeight :: [Cell] -> Int
getMaxHeight cells = maximum $ map getHeight cells

-- beware resulting XY can be out of bounds
getNextXY :: XY -> Dir -> XY
getNextXY (x, y) d = case d of
  North -> (x, succ y)
  East  -> (succ x, y)
  South -> (x, pred y)
  West  -> (pred x, y)

-- beware resulting XYs can be out of bounds
getNextXYs :: XY -> Dir -> Drops -> [XY]
getNextXYs xy dir drops = tail $ foldl red [xy] (show drops)
  where
    red acc _ = acc ++ [getNextXY (last acc) dir]

getNextCells :: Board -> Cell -> Dir -> Drops -> [Maybe Cell]
getNextCells b (Cell xy _) dir drops = map (getCell b) $ getNextXYs xy dir drops

-- up to 4, in each direction
getNeighbors :: Board -> XY -> [Cell]
getNeighbors b xy = cells
  where
    xys = map (getNextXY xy) [North, East, South, West]
    mCells = map (getCell b) xys
    cells = map fromJust $ filter isJust mCells

-- neighbors owned by same player
getValidNeighbors :: Board -> XY -> [Cell]
getValidNeighbors b xy = case getCell b xy of
  Nothing -> []
  Just cell -> case getOwner cell of
    Nothing -> []
    Just owner -> filter (\c -> getOwner c == Just owner) $ getNeighbors b xy
