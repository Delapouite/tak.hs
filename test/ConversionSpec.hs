module ConversionSpec where

import Test.HUnit

import Tak
import Conversion

testsXtoInt = TestList
  [ TestCase $ xToInt 'c' @?= 2 ]

testsToXorY = TestList
  [ TestCase $ toXorY "c" @?= Left 'c'
  , TestCase $ toXorY "4" @?= Right 4
  , TestCase $ toXorY "bdummy" @?= Left 'b'
  ]

testsToStoneType = TestList
  [ TestCase $ toStoneType "s" @?= S
  , TestCase $ toStoneType "F" @?= F
  ]

testsToPlayer = TestList
  [ TestCase $ toPlayer "1" @?= P1
  , TestCase $ toPlayer "2" @?= P2
  ]

testsConversion = TestList
  [ testsXtoInt
  , testsToXorY
  , testsToStoneType
  , testsToPlayer
  ]
