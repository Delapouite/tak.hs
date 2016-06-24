module ParserSpec where

import Test.HUnit

import Tak
import Display
import Parser

testsParseXY = TestList
  [ TestCase $ parseXY "b3" @?= Just ('b', 3)
  , TestCase $ parseXY "B3" @?= Just ('b', 3)
  , TestCase $ parseXY "3b" @?= Nothing
  ]

testsParsePlace = TestList
  [ TestCase $ parsePlace "a" @?= Nothing
  , TestCase $ parsePlace "2" @?= Nothing
  , TestCase $ parsePlace "2a" @?= Nothing

  , TestCase $ parsePlace "a2" @?= Just (F, ('a', 2))
  , TestCase $ parsePlace "Fa2" @?= Just (F, ('a', 2))
  , TestCase $ parsePlace "fa2" @?= Just (F, ('a', 2))
  , TestCase $ parsePlace "Sa2" @?= Just (S, ('a', 2))
  ]

testsParseMove = TestList
  [ TestCase $ parseMove "b2" @?= Nothing
  , TestCase $ parseMove "2b" @?= Nothing
  , TestCase $ parseMove "2bb" @?= Nothing
  , TestCase $ parseMove "2b1" @?= Nothing
  , TestCase $ parseMove "2b1!" @?= Nothing

  , TestCase $ parseMove "b1+" @?= Just (1, ('b', 1), North, 1)
  , TestCase $ parseMove "2b1>" @?= Just (2, ('b', 1), East, 2)
  , TestCase $ parseMove "2b1>11" @?= Just (2, ('b', 1), East, 11)
  ]

testsParseCount = TestList
  [ TestCase $ parseCount "b3>" @?= (1, "b3>")
  , TestCase $ parseCount "4b3>" @?= (4, "b3>")
  ]

testsParseDir = TestList
  [ TestCase $ parseDir '+' @?= Just North
  , TestCase $ parseDir '0' @?= Nothing
  ]

testsParseDrops = TestList
  [ TestCase $ parseDrops 3 "121" @?= 121
  , TestCase $ parseDrops 3 "" @?= 3
  ]

testsParser = TestList
  [ testsParseXY
  , testsParsePlace
  , testsParseMove
  , testsParseCount
  , testsParseDir
  , testsParseDrops
  ]
