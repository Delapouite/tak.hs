module ActionSpec where

import Test.HUnit

import Tak
import Action

testsStackStones = TestList
  [ TestCase $ stackStones ('a', 1) [Stone P1 F] (Cell ('a', 2) []) @?= Cell ('a', 2) []
  , TestCase $ stackStones ('a', 2) [Stone P1 F] (Cell ('a', 2) []) @?= Cell ('a', 2) [Stone P1 F]
  ]

testsUnstackStones = TestList
  [ TestCase $ unstackStones ('a', 1) 1 (Cell ('a', 2) [Stone P2 F, Stone P1 C]) @?= Cell ('a', 2) [Stone P2 F, Stone P1 C]
  , TestCase $ unstackStones ('a', 2) 1 (Cell ('a', 2) [Stone P2 F, Stone P1 C]) @?= Cell ('a', 2) [Stone P2 F]
  , TestCase $ unstackStones ('a', 2) 2 (Cell ('a', 2) [Stone P2 F, Stone P1 C]) @?= Cell ('a', 2) []
  ]

testsZipXYandDrops = TestList
  [ TestCase $ zipXYandDrops (1, ('b', 2), East, 1) @?= [(('c', 2), 1)]
  , TestCase $ zipXYandDrops (3, ('b', 2), East, 21) @?= [(('c', 2), 2), (('d', 2), 1)]
  , TestCase $ zipXYandDrops (3, ('b', 2), East, 111) @?= [(('c', 2), 1), (('d', 2), 1), (('e', 2), 1)]
  ]

testsAction = TestList
  [ testsStackStones
  , testsUnstackStones
  , testsZipXYandDrops
  ]
