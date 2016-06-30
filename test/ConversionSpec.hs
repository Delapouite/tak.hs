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

testsToStoneType = TestList
  [ TestCase $ toStoneType "s" @?= S
  , TestCase $ toStoneType "F" @?= F
  ]

testsConversion = TestList
  [ testsXtoInt
  , testsToXorY
  , testsToStoneType
  ]
