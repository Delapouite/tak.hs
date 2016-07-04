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
data Stone = Stone Player StoneType deriving (Eq, Show)

type Size = Int
type X = Char
type Y = Int
type XY = (X, Y)
-- TODO reverse order?
-- bottom to top (zs)
type Stack = [Stone]

data Cell = Cell XY Stack deriving (Eq, Show)

type Board = [Cell]
type Col = [Cell]
type Row = [Cell]

type Count = Int
type Drops = Int
type Move = (Count, XY, Dir, Drops)

-- PTN: + > - <
data Dir = North | East | South | West deriving (Eq, Show)

type Verb = String
data Command = Command Verb [String]
type Display = String

data Options = Options { optColors :: Bool
                       , optBig :: Bool
                       }

defaultOptions = Options { optColors = False
                         , optBig = False
                         }

data Game = Game { size :: Size
                 , board :: Board
                 , player :: Player
                 , turn :: Int
                 , options :: Options
                 }

minSize = 3 :: Size
maxSize = 8 :: Size

-- axis
xs = ['a'..]

initBoard :: Size -> Board
initBoard size = take (size ^ 2) [Cell (x, y) [] | x <- xs, y <- [1..size]]

stoneCount :: Size -> Int
stoneCount 3 = 10
stoneCount 4 = 15
stoneCount 5 = 21
stoneCount 6 = 30
stoneCount 7 = 40
stoneCount 8 = 50

capCount :: Size -> Int
capCount 8 = 2
capCount 4 = 0
capCount 3 = 0
capCount _ = 1
