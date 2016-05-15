-- https://www.youtube.com/watch?v=iEXkpS-Q9dI

-- Coordinates system according to Portable Tak Notation
-- 3
-- 2
-- 1
--  a b c

import Data.List (transpose)
import Data.List.Split (chunksOf)

data Player = P1 | P2 deriving (Show)

-- Flat, Standing or Cap
data StoneType = F | S | C deriving (Show)

type Stone = (Player, StoneType)

-- bottom to top
type Stack = [Stone]

data Cell = Cell Char Int Stack
instance Show Cell where
  show (Cell x y _) = "."

type Board = [Cell]
type Col = [Cell]
type Row = [Cell]


-- axis
xs = ['a'..]

initBoard :: Int -> Board
initBoard size = take (size ^ 2) [Cell x y [] | x <- xs, y <- [1..size]]


getSize :: Board -> Int
getSize = truncate . sqrt . fromIntegral . length

toCols :: Board -> [Col]
toCols b = chunksOf (getSize b) b

toRows :: Board -> [Row]
toRows = transpose . toCols

showBoard :: Board -> String
showBoard = unlines . reverse . map showRow . toRows

showRow :: Row -> String
showRow = unwords . map show


showBoardWithYAxis :: Board -> String
showBoardWithYAxis = unlines . reverse . map showRowWithY . toRows

showBoardWithAxes :: Board -> String
showBoardWithAxes b = showBoardWithYAxis b ++ "  " ++ showXAxis b

showRowWithY :: Row -> String
showRowWithY r = (show y) ++ " " ++ showRow r
  where
    (Cell _ y _) = head r

showXAxis :: Board -> String
showXAxis b = unwords $ map (\x -> x : []) $ take (getSize b) xs


main = do
  putStrLn "Welcome to Tak.hs"
  putStrLn "Size of the board?"
  size <- getLine
  putStrLn $ showBoardWithAxes $ initBoard (read size :: Int)
