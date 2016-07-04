module Board where

import Data.Char (digitToInt)
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

getMaxHeight :: [Cell] -> Int
getMaxHeight cells = maximum $ map getHeight cells

getMaxX :: Board -> X
getMaxX b = xs !! (getSize b - 1)

-- up to 4, in each direction
getNeighbors :: Board -> XY -> [Cell]
getNeighbors b xy = cells
  where
    xys = map (getNextXY xy) [North, East, South, West]
    mCells = map (getCell b) xys
    cells = map fromJust $ filter isJust mCells

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
getNextXYs xy dir drops = tail $ foldl red [xy] (show drops)
  where
    red acc _ = acc ++ [getNextXY (last acc) dir]

getOwned :: [Cell] -> [Cell]
getOwned = filter (not . isEmpty)

getPlacedByPlayer :: Board -> Player -> [Stone]
getPlacedByPlayer b p = concatMap getOwnStones $ getStacks b
  where
    getOwnStones = filter (\(Stone owner _) -> owner == p)

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

stackStones :: XY -> Stack -> Cell -> Cell
stackStones xy stones c@(Cell xy' zs)
  | xy == xy' = Cell xy (flattenStack zs ++ stones)
  | otherwise = c

unstackStones :: XY -> Count -> Cell -> Cell
unstackStones xy count c@(Cell xy' zs)
  | xy == xy' = Cell xy (reverse (drop count $ reverse zs))
  | otherwise = c

placeStone :: Board -> XY -> Player -> StoneType -> Board
placeStone b xy p st = map (stackStones xy [Stone p st]) b

-- turn all stones to F
flattenStack :: Stack -> Stack
flattenStack = map (\(Stone p t) -> (Stone p F))

moveSubstack :: Board -> Count -> XY -> XY -> Board
moveSubstack b count fromXY toXY = map (stackStones toXY (reverse stones)) b'
  where
    Just (Cell _ zs) = getCell b fromXY
    stones = take count $ reverse zs
    b' = map (unstackStones fromXY count) b

moveStack :: Board -> Move -> Board
moveStack b m@(count, xy, dir, drops) = foldl reducer b $ zipXYandCounts m
  where
    reducer acc (xy, drop) = moveSubstack acc drop xy (getNextXY xy dir)

zipXYandCounts :: Move -> [(XY, Count)]
zipXYandCounts m@(count, xy, dir, drops) = zip xys counts
  where
    xys = init $ xy : getNextXYs xy dir drops
    drops' = map digitToInt (show drops)
    counts = foldl reducer [count] $ init drops'
    reducer acc drop = acc ++ [last acc - drop]
