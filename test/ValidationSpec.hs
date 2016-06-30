module ValidationSpec where

import Test.HUnit

import Tak
import Validation

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

testsIsValidDrops = TestList
  [ TestCase $ isValidDrops 3 3 @?= True
  , TestCase $ isValidDrops 4 3 @?= False
  , TestCase $ isValidDrops 4 121 @?= True
  , TestCase $ isValidDrops 4 122 @?= False
  ]

testsValidation = TestList
  [ testsCanPlace
  , testsIsBoardFull
  , testsIsValidDrops
  ]
