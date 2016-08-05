module BoardSpec where

import Test.HUnit

import Mock

import Tak
import Board
import Display

mockSize = 5

testsInitBoard = TestList
  [ TestCase $ initBoard 2 @?=
    [ Cell ('a', 1) []
    , Cell ('a', 2) []
    , Cell ('b', 1) []
    , Cell ('b', 2) []
    ]
  ]

testsSortBoard = TestList
  [ TestCase $ sortBoard
    [ Cell ('b', 1) []
    , Cell ('a', 1) []
    , Cell ('a', 3) []
    , Cell ('c', 3) []
    , Cell ('b', 2) []
    , Cell ('a', 2) []
    , Cell ('c', 1) []
    , Cell ('c', 2) []
    , Cell ('b', 3) []
    ] @?=
    [ Cell ('a', 1) []
    , Cell ('a', 2) []
    , Cell ('a', 3) []
    , Cell ('b', 1) []
    , Cell ('b', 2) []
    , Cell ('b', 3) []
    , Cell ('c', 1) []
    , Cell ('c', 2) []
    , Cell ('c', 3) []
    ]
  ]

testsGetCell = TestList
  [ TestCase $ getCell mockBoard ('a', 3) @?= Just mockCellA3
  , TestCase $ getCell mockBoard ('a', 4) @?= Nothing
  ]

testsGetCol = TestList
  [ TestCase $ getCol mockBoard 'a' @?=
    [ mockCellA1
    , mockCellA2
    , mockCellA3
    ]
  ]

testsGetMaxHeight = TestList
  [ TestCase $ getMaxHeight [mockCellB3, mockCellC4] @?= 3 ]

testsGetNeighbors = TestList
  [ TestCase $ getNeighbors mockBoard ('a', 1) @?=
    [ mockCellA2
    , Cell ('b', 1) []
    ]
  , TestCase $ getNeighbors mockBoard ('b', 1) @?=
    [ Cell ('b', 2) []
    , Cell ('c', 1) []
    , mockCellA1
    ]
  , TestCase $ getNeighbors mockBoard ('b', 2) @?=
    [ mockCellB3
    , Cell ('c', 2) []
    , Cell ('b', 1) []
    , mockCellA2
    ]
  ]

testsGetNextXY = TestList
  [ TestCase $ getNextXY mockSize ('b', 2) North @?= Just ('b', 3)
  , TestCase $ getNextXY mockSize ('b', 2) East  @?= Just ('c', 2)
  , TestCase $ getNextXY mockSize ('b', 2) South @?= Just ('b', 1)
  , TestCase $ getNextXY mockSize ('b', 2) West  @?= Just ('a', 2)
  , TestCase $ getNextXY mockSize ('a', 1) West  @?= Nothing
  , TestCase $ getNextXY mockSize ('a', 1) South @?= Nothing
  , TestCase $ getNextXY mockSize ('e', 5) North @?= Nothing
  , TestCase $ getNextXY mockSize ('e', 5) East  @?= Nothing
  ]

testsGetNextXYs = TestList
  [ TestCase $ getNextXYs mockSize ('b', 2) East [1] @?= [('c', 2)]
  , TestCase $ getNextXYs mockSize ('b', 2) East [2] @?= [('c', 2)]
  , TestCase $ getNextXYs mockSize ('b', 2) East [2, 1] @?= [('c', 2), ('d', 2)]
  , TestCase $ getNextXYs mockSize ('b', 2) North [2, 1] @?= [('b', 3), ('b', 4)]
  , TestCase $ getNextXYs mockSize ('b', 2) North [2, 1, 2] @?= [('b', 3), ('b', 4), ('b', 5)]
  -- out of bounds
  , TestCase $ getNextXYs mockSize ('b', 2) South [2, 1] @?= [('b', 1)]
  , TestCase $ getNextXYs mockSize ('b', 2) North [2, 1, 2, 1] @?= [('b', 3), ('b', 4), ('b', 5)]
  ]

testsGetOwned = TestList
  [ TestCase $ getOwned mockBoard @?=
    [mockCellA1, mockCellA2, mockCellA3, mockCellB3]
  ]

testsGetRow = TestList
  [ TestCase $ getRow mockBoard 2 @?=
    [ mockCellA2
    , Cell ('b', 2) []
    , Cell ('c', 2) []
    ]
  ]

testsGetSize = TestCase $ assertEqual "Should return side length of the Board" exp act
  where
    exp = 5
    act = getSize $ initBoard exp

testsGetValidNeighbors = TestList
  [ TestCase $ getValidNeighbors mockBoard ('a', 1) @?= [ mockCellA2 ]
  , TestCase $ getValidNeighbors mockBoard ('b', 1) @?= []
  , TestCase $ getValidNeighbors mockBoard ('b', 3) @?= []
  , TestCase $ getValidNeighbors mockBoard ('b', 6) @?= []
  ]

testsIsBoardFull = TestList
  [ TestCase $ False @=? isBoardFull
     [ Cell ('a', 1) []
     , Cell ('a', 2) []
     , Cell ('b', 1) []
     , Cell ('b', 2) []
     ]
  , TestCase $ False @=? isBoardFull
     [ Cell ('a', 1) [Stone P1 F]
     , Cell ('a', 2) []
     , Cell ('b', 1) [Stone P2 C]
     , Cell ('b', 2) []
     ]
  , TestCase $ True @=? isBoardFull
     [ Cell ('a', 1) [Stone P1 F]
     , Cell ('a', 2) [Stone P1 F]
     , Cell ('b', 1) [Stone P2 C]
     , Cell ('b', 2) [Stone P2 S]
     ]
  ]

testsToCols = TestList
  [ TestCase $ toCols
    [ Cell ('a', 1) []
    , Cell ('a', 2) []
    , Cell ('b', 1) []
    , Cell ('b', 2) []
    ] @?=
    [
      [ Cell ('a', 1) []
      , Cell ('a', 2) []
      ]
      ,
      [ Cell ('b', 1) []
      , Cell ('b', 2) []
      ]
    ]
  ]

testsToRows = TestList
  [ TestCase $ toRows
    [ Cell ('a', 1) []
    , Cell ('a', 2) []
    , Cell ('b', 1) []
    , Cell ('b', 2) []
    ] @?=
    [
      [ Cell ('a', 1) []
      , Cell ('b', 1) []
      ]
      ,
      [ Cell ('a', 2) []
      , Cell ('b', 2) []
      ]
    ]
  ]

-- stack

mockBoard1 =
  [ Cell ('a', 2) [Stone P2 F]
  , Cell ('b', 2) [Stone P1 S, Stone P2 F, Stone P1 F]
  , Cell ('a', 1) []
  , Cell ('b', 1) []
  ]

testsMoveSubstack = TestList
  [ TestCase $ moveSubstack mockBoard1 1 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P1 S, Stone P2 F]
    , Cell ('b', 2) [Stone P2 F, Stone P1 F]
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ moveSubstack mockBoard1 2 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P1 S, Stone P2 F, Stone P2 F]
    , Cell ('b', 2) [Stone P1 F]
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ moveSubstack mockBoard1 3 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P1 S, Stone P2 F, Stone P1 F, Stone P2 F]
    , Cell ('b', 2) []
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  ]

testsZipXYandCounts = TestList
  [ TestCase $ zipXYandCounts mockSize (1, ('b', 2), East, [1]) @?= [(('b', 2), 1)]
  , TestCase $ zipXYandCounts mockSize (3, ('b', 2), East, [2, 1]) @?= [(('b', 2), 3), (('c', 2), 1)]
  , TestCase $ zipXYandCounts mockSize (3, ('b', 2), North, [2, 1]) @?= [(('b', 2), 3), (('b', 3), 1)]
  , TestCase $ zipXYandCounts mockSize (3, ('b', 2), East, [1, 1, 1]) @?= [(('b', 2), 3), (('c', 2), 2), (('d', 2), 1)]
  , TestCase $ zipXYandCounts mockSize (4, ('b', 2), East, [1, 2, 1]) @?= [(('b', 2), 4), (('c', 2), 3), (('d', 2), 1)]
  ]

testsBoard = TestList
  [ testsInitBoard
  , testsSortBoard
  , testsGetCell
  , testsGetCol
  , testsGetMaxHeight
  , testsGetNeighbors
  , testsGetNextXY
  , testsGetNextXYs
  , testsGetOwned
  , testsGetRow
  , testsGetSize
  , testsGetValidNeighbors
  , testsIsBoardFull
  , testsToCols
  , testsToRows
  , testsMoveSubstack
  , testsZipXYandCounts
  ]
