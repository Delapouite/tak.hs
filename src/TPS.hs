module TPS where

import Data.Char (digitToInt, isAlpha, isDigit, toLower, toUpper)
import Data.List (intercalate, intersperse)
import Data.List.Split (splitOn, split, oneOf, condense)
import Text.Read (readMaybe)

import Tak
import Board
import Conversion

-- [TPS "x3,12,2S/x,22S,22C,11,21/121,212,12,1121C,1212S/21S,1,21,211S,12S/x,21S,2,x2 1 26"]
parseTPS :: String -> Game
parseTPS tps = Game { size = getSize b'
                    , board = b'
                    , player = p'
                    , turn = read t
                    , options = defaultOptions
                    }
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

-- TODO wut? toCols
toTPSBoard :: Board -> String
toTPSBoard b = intercalate "/" $ map toTPSRow (toCols b)

-- x5
-- x3,12,2S
parseTPSRow :: String -> Y -> Row
parseTPSRow tps y = cells
  where
    tpsRow = intercalate "," . map parseTPSX $ splitOn "," tps
    tpsCell =  splitOn "," tpsRow
    cells = [parseTPSCell c (x, y) | (c, x) <- zip tpsCell $ take (length tpsCell) xs]

toTPSRow :: Row -> String
toTPSRow r = intercalate "," $ map toTPSX parts'
  where
    -- [["12S", "2"], ["x", "x"], ["2"], ["x"]]
    parts = split (condense $ oneOf ["x"]) $ map toTPSCell r
    -- ["12S", "2", "xx", "2", "x"]
    parts' = concatMap (\g -> if "x" `elem` g then [concat g] else g) parts

-- x5 → x,x,x,x,x
parseTPSX :: String -> String
parseTPSX ['x', n] = intersperse ',' $ replicate (digitToInt n) 'x'
parseTPSX tps = tps

toTPSX :: String -> String
toTPSX str
  | "x" == str = "x"
  | 'x' `elem` str = 'x' : show (length str)
  | otherwise = str

parseTPSCell :: String -> XY -> Cell
parseTPSCell tps xy = Cell xy (parseTPSStack tps)

toTPSCell :: Cell -> String
toTPSCell (Cell _ zs) = toTPSStack zs

parseTPSStack :: String -> Stack
parseTPSStack "x" = []
parseTPSStack tps = stones'
  where
    -- Stack are top to bottom
    tps' = reverse tps
    topSType = if head tps' `elem` "SC" then read [head tps'] :: StoneType else F
    chars = if topSType /= F then tail tps' else tps'
    stones = map (\c -> Stone (toPlayer [c]) F) chars
    stones' = map (\(Stone p _) -> Stone p topSType) [head stones] ++ tail stones

toTPSStack :: Stack -> String
toTPSStack [] = "x"
toTPSStack zs = concatMap toTPSStone $ reverse zs

toTPSStone :: Stone -> String
toTPSStone (Stone p t) = case t of
  F -> toTPSPlayer p
  _ -> toTPSPlayer p ++ show t

toTPSPlayer :: Player -> String
toTPSPlayer P1 = "1"
toTPSPlayer P2 = "2"
