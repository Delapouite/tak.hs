module ConversionSpec where

import Test.HUnit

import Tak
import Conversion

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

testsConversion = TestList
  [ testsGetSize
  , testsXtoInt
  , testsToXorY
  , testsGetHeight
  ]
