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
  show (Cell x y _) = x : show y

type Board = [Cell]
type Col = [Cell]
type Row = [Cell]


initBoard :: Int -> Board
initBoard size = take (size ^ 2) [Cell x y [] | x <- ['a'..], y <- [1..size]]

toCols :: Board -> [Col]
toCols b = chunksOf size b

toRows :: Board -> [Row]
toRows = transpose . toCols

showBoard :: Board -> String
showBoard = unlines . reverse . map showRow . toRows

showRow :: Row -> String
showRow = unwords . map show

size = 5


main = do
  putStrLn "Welcome to Tak.hs"
  putStrLn $ showBoard $ initBoard size
