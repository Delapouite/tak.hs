module ValidationSpec where

import Test.HUnit

import Tak
import Validation

testsIsValidDrops = TestList
  [ TestCase $ isValidDrops 3 3 @?= True
  , TestCase $ isValidDrops 4 3 @?= False
  , TestCase $ isValidDrops 4 121 @?= True
  , TestCase $ isValidDrops 4 122 @?= False
  ]

testsCanPlace = TestList
  [ TestCase $ True @=? canPlace
     [ Cell ('a', 1) []
     , Cell ('a', 2) []
     , Cell ('b', 1) []
     , Cell ('b', 2) []
     ]
     ('a', 2)
  , TestCase $ False @=? canPlace
     [ Cell ('a', 1) []
     , Cell ('a', 2) []
     , Cell ('b', 1) []
     , Cell ('b', 2) []
     ]
     ('c', 3)
  , TestCase $ False @=? canPlace
     [ Cell ('a', 1) []
     , Cell ('a', 2) [Stone P1 F]
     , Cell ('b', 1) []
     , Cell ('b', 2) []
     ]
     ('a', 2)
  ]

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
  [ testsIsValidDrops
  , testsCanPlace
  , testsIsBoardFull
  , testsIsToppable
  , testsHasCap
  ]
