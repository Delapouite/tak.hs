module Conversion where

import Data.Char (isDigit, ord, toUpper)
import Data.List (find, transpose)
import Data.List.Split (chunksOf)

import Tak

getSize :: Board -> Int
getSize = truncate . sqrt . fromIntegral . length

getPlayer :: Game -> Player
getPlayer g = if turn g `mod` 2 == 0 then P2 else P1

getPlacedByPlayer :: Game -> Player -> [Stone]
getPlacedByPlayer g p = concatMap getOwnStones $ getStacks g
  where
    getOwnStones = filter (\(Stone owner _) -> owner == p)

getPlaced :: Game -> [Stone]
getPlaced g = getPlacedByPlayer g P1 ++ getPlacedByPlayer g P2

getPlacedByPlayerAndType :: Game -> Player -> StoneType -> [Stone]
getPlacedByPlayerAndType g p st = filter (\(Stone _ t) -> t == st) $ getPlacedByPlayer g p

getCell :: Board -> XY -> Maybe Cell
getCell b xy = find (\(Cell xy' _) -> xy == xy') b

getStacks :: Game -> [Stack]
getStacks g = map (\(Cell _ zs) -> zs) $ board g

getStackHeight :: Cell -> Int
getStackHeight (Cell _ zs) = length zs

getTallerStackHeight :: [Cell] -> Int
getTallerStackHeight cells = maximum $ map getStackHeight cells

-- TODO maybeLast?
getTopStone :: Cell -> Maybe Stone
getTopStone (Cell _ zs)
  | null zs   = Nothing
  | otherwise = Just $ last zs

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

-- 97 = ASCII 'a'
xToInt :: X -> Int
xToInt x = ord x - 97

toXorY :: String -> Either X Y
toXorY arg = if isDigit $ head arg
  then Right (read [head arg] :: Int)
  else Left  (head arg)

toStoneType :: String -> StoneType
toStoneType arg = read (map toUpper arg) :: StoneType

toCols :: Board -> [Col]
toCols b = chunksOf (getSize b) b

toRows :: Board -> [Row]
toRows = transpose . toCols


