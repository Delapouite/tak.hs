module Mock where

import Tak

mockStackS1 = [Stone P1 S, Stone P2 F, Stone P1 F]
mockStackC1 = [Stone P1 C, Stone P2 F]
mockStackF2 = [Stone P2 F]

mockCellA1 = Cell ('a', 1) mockStackS1
mockCellA2 = Cell ('a', 2) mockStackS1
mockCellA3 = Cell ('a', 3) mockStackF2
mockCellB3 = Cell ('b', 3) mockStackS1
mockCellC4 = Cell ('c', 4) mockStackC1

mockBoard =
  [ mockCellA1
  , mockCellA2
  , mockCellA3
  , Cell ('b', 1) []
  , Cell ('b', 2) []
  , mockCellB3
  , Cell ('c', 1) []
  , Cell ('c', 2) []
  , Cell ('c', 3) []
  ]

mockSize = 5 :: Int
