module ConversionSpec where

import Test.HUnit

import Tak
import Conversion

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

testsConversion = TestList
  [ testsGetSize
  , testsXtoInt
  , testsToXorY
  ]
