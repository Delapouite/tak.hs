module ParserSpec where

import Test.HUnit

import Tak
import Parser

testParseXY1 = TestCase $ assertEqual "Should return the correct XY" exp act
  where
    exp = Just ('b', 3)
    act = parseXY "b3"

testParseXY2 = TestCase $ assertEqual "Should return the correct XY" exp act
  where
    exp = Nothing
    act = parseXY "3b"

testParseXY3 = TestCase $ assertEqual "Should return the correct XY" exp act
  where
    exp = Just ('b', 3)
    act = parseXY "B3"

testsParseXY = TestList [ testParseXY1, testParseXY2, testParseXY3 ]

testsParser = TestList
  [ testsParseXY ]
