module PlaceSpec where

import Test.HUnit

import Tak
import Place

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


testsPlace = TestList
  [ testsCanPlace
  ]
