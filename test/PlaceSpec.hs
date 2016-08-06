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

testsCapsInDeck = TestList
  -- no caps for 3x3
  [ TestCase $ False @=? capsInDeck
     [ Cell ('a', 1) [] , Cell ('a', 2) [] , Cell ('a', 3) []
     , Cell ('b', 1) [] , Cell ('b', 2) [] , Cell ('b', 3) []
     , Cell ('c', 1) [] , Cell ('c', 2) [] , Cell ('c', 3) []
     ]
     P1
  , TestCase $ True @=? capsInDeck
     [ Cell ('a', 1) [] , Cell ('a', 2) [] , Cell ('a', 3) [], Cell ('a', 4) [], Cell ('a', 5) []
     , Cell ('b', 1) [] , Cell ('b', 2) [] , Cell ('b', 3) [], Cell ('b', 4) [], Cell ('b', 5) []
     , Cell ('c', 1) [] , Cell ('c', 2) [] , Cell ('c', 3) [], Cell ('c', 4) [], Cell ('c', 5) []
     , Cell ('d', 1) [] , Cell ('d', 2) [] , Cell ('d', 3) [], Cell ('d', 4) [], Cell ('d', 5) []
     , Cell ('e', 1) [] , Cell ('e', 2) [] , Cell ('e', 3) [], Cell ('e', 4) [], Cell ('e', 5) []
     ]
     P1
  , TestCase $ True @=? capsInDeck
     [ Cell ('a', 1) [] , Cell ('a', 2) [Stone P1 F] , Cell ('a', 3) [], Cell ('a', 4) [], Cell ('a', 5) []
     , Cell ('b', 1) [] , Cell ('b', 2) [Stone P2 C] , Cell ('b', 3) [], Cell ('b', 4) [], Cell ('b', 5) []
     , Cell ('c', 1) [] , Cell ('c', 2) [] , Cell ('c', 3) [], Cell ('c', 4) [], Cell ('c', 5) []
     , Cell ('d', 1) [] , Cell ('d', 2) [] , Cell ('d', 3) [], Cell ('d', 4) [], Cell ('d', 5) []
     , Cell ('e', 1) [] , Cell ('e', 2) [] , Cell ('e', 3) [], Cell ('e', 4) [], Cell ('e', 5) []
     ]
     P1
  -- cap used
  , TestCase $ False @=? capsInDeck
     [ Cell ('a', 1) [] , Cell ('a', 2) [Stone P1 F] , Cell ('a', 3) [], Cell ('a', 4) [], Cell ('a', 5) []
     , Cell ('b', 1) [] , Cell ('b', 2) [Stone P2 C] , Cell ('b', 3) [], Cell ('b', 4) [], Cell ('b', 5) []
     , Cell ('c', 1) [] , Cell ('c', 2) [Stone P1 C] , Cell ('c', 3) [], Cell ('c', 4) [], Cell ('c', 5) []
     , Cell ('d', 1) [] , Cell ('d', 2) [] , Cell ('d', 3) [], Cell ('d', 4) [], Cell ('d', 5) []
     , Cell ('e', 1) [] , Cell ('e', 2) [] , Cell ('e', 3) [], Cell ('e', 4) [], Cell ('e', 5) []
     ]
     P1
  ]

testsPlace = TestList
  [ testsCanPlace
  , testsCapsInDeck
  ]
