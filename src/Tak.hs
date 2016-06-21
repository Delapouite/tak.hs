module Tak where

-- Coordinates system according to Portable Tak Notation
-- 3
-- 2
-- 1
--  a b c

-- PTN
-- Place flat stone at a1: a1
-- Place capstone at b4: Cb4
-- Place standing stone at d3: Sd3


data Player = P1 | P2 deriving (Eq, Show)

-- Flat, Standing or Cap
data StoneType = F | S | C deriving (Eq, Show, Read)

-- P1 in UpperCase, P2 in LowerCase
data Stone = Stone Player StoneType

type X = Char
type Y = Int
type XY = (X, Y)
-- TODO reverse order?
-- bottom to top (zs)
type Stack = [Stone]

data Cell = Cell XY Stack

type Board = [Cell]
type Col = [Cell]
type Row = [Cell]
type Drops = Int
type Move = (Int, XY, Dir, Drops)

-- PTN: + > - <
data Dir = North | East | South | West deriving (Show)

type Verb = String
data Action = Action Verb [String]
type Display = String

data Game = Game { size :: Int
                 , board :: Board
                 , turn :: Int
                 }

minSize = 3 :: Int
maxSize = 8 :: Int

-- axis
xs = ['a'..]

initBoard :: Int -> Board
initBoard size = take (size ^ 2) [Cell (x, y) [] | x <- xs, y <- [1..size]]

stoneCount :: Int -> Int
stoneCount 3 = 10
stoneCount 4 = 15
stoneCount 5 = 21
stoneCount 6 = 30
stoneCount 7 = 40
stoneCount 8 = 50

capCount :: Int -> Int
capCount 8 = 2
capCount 4 = 0
capCount 3 = 0
capCount _ = 1
