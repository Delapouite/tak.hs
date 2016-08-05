module Board where

import Data.List (find, sortBy, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)

import Tak
import Cell
import Conversion

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
  xys = map (getNextXY xy) [North, East, South, West]
  mCells = map (getCell b) xys
  in catMaybes mCells

getNextCells :: Board -> Cell -> Dir -> Drops -> [Maybe Cell]
getNextCells b (Cell xy _) dir drops = map (getCell b) $ getNextXYs xy dir drops

-- beware resulting XY can be out of bounds
getNextXY :: XY -> Dir -> XY
getNextXY (x, y) d = case d of
  North -> (x, succ y)
  East  -> (succ x, y)
  South -> (x, pred y)
  West  -> (pred x, y)

-- beware resulting XYs can be out of bounds
getNextXYs :: XY -> Dir -> Drops -> [XY]
getNextXYs xy dir drops = let
  reducer acc _ = acc ++ [getNextXY (last acc) dir]
  in tail $ foldl reducer [xy] drops

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
    Just owner -> filter (\c -> getOwner c == Just owner) $ getNeighbors b xy

isBoardFull :: Board -> Bool
isBoardFull = not . any isEmpty

toCols :: Board -> [Col]
toCols b = chunksOf (getSize b) b

toRows :: Board -> [Row]
toRows = transpose . toCols

-- stack

pushStones :: XY -> Stack -> Cell -> Cell
pushStones xy stones c@(Cell xy' zs)
  | xy == xy' = Cell xy (stones ++ flattenStack zs)
  | otherwise = c

popStones :: XY -> Count -> Cell -> Cell
popStones xy count c@(Cell xy' zs)
  | xy == xy' = Cell xy (drop count zs)
  | otherwise = c

placeStone :: Board -> XY -> Player -> StoneType -> Board
placeStone b xy p st = map (pushStones xy [Stone p st]) b

-- turn all stones to F
flattenStack :: Stack -> Stack
flattenStack = map (\(Stone p t) -> (Stone p F))

moveSubstack :: Board -> Count -> XY -> XY -> Board
moveSubstack b count fromXY toXY = let
  Just (Cell _ zs) = getCell b fromXY
  stones = take count zs
  b' = map (popStones fromXY count) b
  in map (pushStones toXY stones) b'

moveStack :: Board -> Move -> Board
moveStack b m@(count, xy, dir, _) = let
  reducer acc (xy, drop) = moveSubstack acc drop xy (getNextXY xy dir)
  in foldl reducer b $ zipXYandCounts m

zipXYandCounts :: Move -> [(XY, Count)]
zipXYandCounts (count, xy, dir, drops) = let
  xys = init $ xy : getNextXYs xy dir drops
  counts = foldl reducer [count] $ init drops
  reducer acc drop = acc ++ [last acc - drop]
  in zip xys counts
