module ValidationSpec where

import Test.HUnit

import Tak
import Validation

testsIsBoardFull = TestList
  [ TestCase $ False @=? isBoardFull
     [ Cell ('a', 1) []
     , Cell ('a', 2) []
     , Cell ('b', 1) []
     , Cell ('b', 2) []
     ]
  , TestCase $ False @=? isBoardFull
     [ Cell ('a', 1) [Stone P1 F]
     , Cell ('a', 2) []
     , Cell ('b', 1) [Stone P2 C]
     , Cell ('b', 2) []
     ]
  , TestCase $ True @=? isBoardFull
     [ Cell ('a', 1) [Stone P1 F]
     , Cell ('a', 2) [Stone P1 F]
     , Cell ('b', 1) [Stone P2 C]
     , Cell ('b', 2) [Stone P2 S]
     ]
  ]

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
  [ testsIsBoardFull
  , testsIsToppable
  , testsHasCap
  ]
