module ValidationSpec where

import Test.HUnit

import Tak
import Validation

testsIsToppable = TestList
  [ TestCase $ isToppable (Cell ('a', 1) []) @?= True
  , TestCase $ isToppable (Cell ('a', 1) [Stone P1 F]) @?= True
  , TestCase $ isToppable (Cell ('a', 1) [Stone P1 F, Stone P1 F]) @?= True
  , TestCase $ isToppable (Cell ('a', 1) [Stone P1 S]) @?= False
  , TestCase $ isToppable (Cell ('a', 1) [Stone P1 C]) @?= False
  ]

testsHasCap = TestList
  [ TestCase $ hasCap (Cell ('a', 1) []) @?= False
  , TestCase $ hasCap (Cell ('a', 1) [Stone P1 S]) @?= False
  , TestCase $ hasCap (Cell ('a', 1) [Stone P1 C]) @?= True
  ]

testsValidation = TestList
  [ testsIsToppable
  , testsHasCap
  ]
