module DisplaySpec where

import Test.HUnit

import Mock

import Tak
import Display

testsShowYAxis = TestList
  [ TestCase $ showYAxis mockBoard @?= "3 2 1"
  ]

testsShowXAxis = TestList
  [ TestCase $ showXAxis mockBoard @?= "a b c"
  ]

testsDisplay = TestList
  [ testsShowYAxis
  , testsShowXAxis
  ]

