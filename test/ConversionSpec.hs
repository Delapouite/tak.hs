module ConversionSpec where

import Test.HUnit

import Tak
import Conversion
import Display

mockStack1 = [Stone P1 F, Stone P2 F, Stone P1 S]
mockStack2 = [Stone P2 F, Stone P1 C]

mockCell1 = Cell ('b', 3) mockStack1
mockCell2 = Cell ('c', 4) mockStack2

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

testsGetHeight = TestList
  [ TestCase $ getHeight mockCell1 @?= 3
  , TestCase $ getHeight mockCell2 @?= 2
  ]

testsGetTallestHeight = TestList
  [ TestCase $ getTallestHeight [mockCell1, mockCell2] @?= 3 ]

testsGetTopStone = TestList
  [ TestCase $ getTopStone (Cell ('b', 2) []) @?= Nothing
  , TestCase $ getTopStone (Cell ('b', 2) [Stone P1 F]) @?= Just (Stone P1 F)
  , TestCase $ getTopStone (Cell ('b', 2) [Stone P1 F, Stone P2 C]) @?= Just (Stone P2 C)
  ]

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

testsConversion = TestList
  [ testsXtoInt
  , testsToXorY
  , testsToStoneType
  , testsGetSize
  , testsGetHeight
  , testsGetTallestHeight
  , testsGetTopStone
  , testsGetNextXY
  , testsGetNextXYs
  ]
