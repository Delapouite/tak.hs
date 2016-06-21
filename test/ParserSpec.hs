module ParserSpec where

import Test.HUnit

import Tak
import Parser

testsParseXY = TestList
  [ TestCase $ parseXY "b3" @?= Just ('b', 3)
  , TestCase $ parseXY "B3" @?= Just ('b', 3)
  , TestCase $ parseXY "3b" @?= Nothing
  ]

testsParseCount = TestList
  [ TestCase $ parseCount "b3>" @?= (1, "b3>")
  , TestCase $ parseCount "4b3>" @?= (4, "b3>")
  ]

testsParseDir = TestList
  [ TestCase $ parseDir '+' @?= Just North
  , TestCase $ parseDir '0' @?= Nothing
  ]

testsParseDrops= TestList
  [ TestCase $ parseDrops 3 "121" @?= 121
  , TestCase $ parseDrops 3 "" @?= 3
  ]

testsParser = TestList
  [ testsParseXY
  , testsParseCount
  , testsParseDir
  , testsParseDrops
  ]
