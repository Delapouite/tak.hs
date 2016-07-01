module BoardSpec where

import Test.HUnit

import Tak
import Board
import Display

mockStackS1 = [Stone P1 F, Stone P2 F, Stone P1 S]
mockStackC1 = [Stone P2 F, Stone P1 C]
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


testsGetCol = TestList
  [ TestCase $ getCol mockBoard 'a' @?=
    [ mockCellA1
    , mockCellA2
    , mockCellA3
    ]
  ]

testsGetMaxHeight = TestList
  [ TestCase $ getMaxHeight [mockCellB3, mockCellC4] @?= 3 ]

testsGetMaxX = TestList
  [ TestCase $ getMaxX mockBoard @?= 'c' ]

testsGetNextXY = TestList
  [ TestCase $ getNextXY ('b', 2) North @?= ('b', 3)
  , TestCase $ getNextXY ('b', 2) East  @?= ('c', 2)
  , TestCase $ getNextXY ('b', 2) South @?= ('b', 1)
  , TestCase $ getNextXY ('b', 2) West  @?= ('a', 2)
  ]

testsGetNextXYs = TestList
  [ TestCase $ getNextXYs ('b', 2) East 1 @?= [('c', 2)]
  , TestCase $ getNextXYs ('b', 2) East 2 @?= [('c', 2)]
  , TestCase $ getNextXYs ('b', 2) East 21 @?= [('c', 2), ('d', 2)]
  , TestCase $ getNextXYs ('b', 2) North 21 @?= [('b', 3), ('b', 4)]
  , TestCase $ getNextXYs ('b', 2) North 212 @?= [('b', 3), ('b', 4), ('b', 5)]
  ]

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

-- stack

mockBoard1 =
  [ Cell ('a', 2) [Stone P2 F]
  , Cell ('b', 2) [Stone P1 F, Stone P2 F, Stone P1 S]
  , Cell ('a', 1) []
  , Cell ('b', 1) []
  ]

testsStackStones = TestList
  [ TestCase $ stackStones ('a', 1) [Stone P1 F] (Cell ('a', 2) []) @?= Cell ('a', 2) []
  , TestCase $ stackStones ('a', 2) [Stone P1 F] (Cell ('a', 2) []) @?= Cell ('a', 2) [Stone P1 F]
  , TestCase $ stackStones ('a', 2) [Stone P1 F, Stone P1 S] (Cell ('a', 2) []) @?= Cell ('a', 2) [Stone P1 F, Stone P1 S]
  ]

testsUnstackStones = TestList
  [ TestCase $ unstackStones ('a', 1) 1 (Cell ('a', 2) [Stone P2 F, Stone P1 C]) @?= Cell ('a', 2) [Stone P2 F, Stone P1 C]
  , TestCase $ unstackStones ('a', 2) 1 (Cell ('a', 2) [Stone P2 F, Stone P1 C]) @?= Cell ('a', 2) [Stone P2 F]
  , TestCase $ unstackStones ('a', 2) 2 (Cell ('a', 2) [Stone P2 F, Stone P1 C]) @?= Cell ('a', 2) []
  , TestCase $ unstackStones ('a', 2) 2 (Cell ('a', 2) [Stone P2 F, Stone P1 F, Stone P1 C]) @?= Cell ('a', 2) [Stone P2 F]
  ]

testsMoveSubstack = TestList
  [ TestCase $ moveSubstack mockBoard1 1 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P2 F, Stone P1 S]
    , Cell ('b', 2) [Stone P1 F, Stone P2 F]
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ moveSubstack mockBoard1 2 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P2 F, Stone P2 F, Stone P1 S]
    , Cell ('b', 2) [Stone P1 F]
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ moveSubstack mockBoard1 3 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P2 F, Stone P1 F, Stone P2 F, Stone P1 S]
    , Cell ('b', 2) []
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  ]

testsZipXYandCounts = TestList
  [ TestCase $ zipXYandCounts (1, ('b', 2), East, 1) @?= [(('b', 2), 1)]
  , TestCase $ zipXYandCounts (3, ('b', 2), East, 21) @?= [(('b', 2), 3), (('c', 2), 1)]
  , TestCase $ zipXYandCounts (3, ('b', 2), East, 111) @?= [(('b', 2), 3), (('c', 2), 2), (('d', 2), 1)]
  , TestCase $ zipXYandCounts (4, ('b', 2), East, 121) @?= [(('b', 2), 4), (('c', 2), 3), (('d', 2), 1)]
  ]

testsBoard = TestList
  [ testsGetCol
  , testsGetMaxHeight
  , testsGetMaxX
  , testsGetNeighbors
  , testsGetNextXY
  , testsGetNextXYs
  , testsGetRow
  , testsGetSize
  , testsGetValidNeighbors
  , testsIsBoardFull
  , testsStackStones
  , testsUnstackStones
  , testsMoveSubstack
  , testsZipXYandCounts
  ]
