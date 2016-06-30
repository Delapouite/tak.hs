module ConversionSpec where

import Test.HUnit

import Tak
import Conversion
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

testsXtoInt = TestList
  [ TestCase $ xToInt 'c' @?= 2 ]

testsToXorY = TestList
  [ TestCase $ toXorY "c" @?= Left 'c'
  , TestCase $ toXorY "4" @?= Right 4
  ]

testsToStoneType = TestList
  [ TestCase $ toStoneType "s" @?= S
  , TestCase $ toStoneType "F" @?= F
  ]

testsGetSize = TestCase $ assertEqual "Should return side length of the Board" exp act
  where
    exp = 5
    act = getSize $ initBoard exp

testsGetMaxHeight = TestList
  [ TestCase $ getMaxHeight [mockCellB3, mockCellC4] @?= 3 ]

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

testsGetValidNeighbors = TestList
  [ TestCase $ getValidNeighbors mockBoard ('a', 1) @?= [ mockCellA2 ]
  , TestCase $ getValidNeighbors mockBoard ('b', 1) @?= []
  , TestCase $ getValidNeighbors mockBoard ('b', 3) @?= []
  , TestCase $ getValidNeighbors mockBoard ('b', 6) @?= []
  ]

testsGetCol = TestList
  [ TestCase $ getCol mockBoard 'a' @?=
    [ mockCellA1
    , mockCellA2
    , mockCellA3
    ]
  ]

testsGetRow = TestList
  [ TestCase $ getRow mockBoard 2 @?=
    [ mockCellA2
    , Cell ('b', 2) []
    , Cell ('c', 2) []
    ]
  ]

testsGetMaxX = TestList
  [ TestCase $ getMaxX mockBoard @?= 'c' ]

testsConversion = TestList
  [ testsXtoInt
  , testsToXorY
  , testsToStoneType
  , testsGetSize
  , testsGetMaxHeight
  , testsGetNextXY
  , testsGetNextXYs
  , testsGetNeighbors
  , testsGetValidNeighbors
  , testsGetCol
  , testsGetRow
  , testsGetMaxX
  ]
