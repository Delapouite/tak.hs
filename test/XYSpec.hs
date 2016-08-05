module XYSpec where

import Test.HUnit

import Tak
import XY

testsIsValidX = TestList
  [ TestCase $ isValidX 4 'a' @?= True
  , TestCase $ isValidX 4 'd' @?= True
  , TestCase $ isValidX 4 'e' @?= False
  ]

testsIsValidY = TestList
  [ TestCase $ isValidY 4 1 @?= True
  , TestCase $ isValidY 4 4 @?= True
  , TestCase $ isValidY 4 5 @?= False
  ]

testsIsValidXY = TestList
  [ TestCase $ isValidXY 4 ('a', 1) @?= True
  , TestCase $ isValidXY 4 ('a', 4) @?= True
  , TestCase $ isValidXY 4 ('d', 1) @?= True
  , TestCase $ isValidXY 4 ('d', 4) @?= True
  , TestCase $ isValidXY 4 ('e', 1) @?= False
  , TestCase $ isValidXY 4 ('a', 5) @?= False
  ]

testsIsValidSize = TestList
  [ TestCase $ isValidSize 4 @?= True
  , TestCase $ isValidSize 2 @?= False
  , TestCase $ isValidSize 9 @?= False
  ]

testsXY = TestList
  [ testsIsValidX
  , testsIsValidY
  , testsIsValidXY
  , testsIsValidSize
  ]
