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

-- top to bottom, often refered as zs through the code (3rd dimension)
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

stoneCount :: Size -> Int
stoneCount s = case s of
  3 -> 10
  4 -> 15
  5 -> 21
  6 -> 30
  7 -> 40
  8 -> 50

capCount :: Size -> Int
capCount s = case s of
  8 -> 2
  4 -> 0
  3 -> 0
  _ -> 1
