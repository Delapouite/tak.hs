module ParserSpec where

import Test.HUnit

import Tak
import Parser

testsParseXY = TestList
  [ TestCase $ parseXY "b3" @?= Just ('b', 3)
  , TestCase $ parseXY "B3" @?= Just ('b', 3)
  , TestCase $ parseXY "3b" @?= Nothing
  ]

testsParser = TestList
  [ testsParseXY ]
