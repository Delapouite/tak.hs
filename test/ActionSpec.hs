module ActionSpec where

import Test.HUnit

import Tak
import Action

mockBoard1 =
  [ Cell ('a', 2) [Stone P2 F]
  , Cell ('b', 2) [Stone P1 F, Stone P1 F]
  , Cell ('a', 1) []
  , Cell ('b', 1) []
  ]

testsStackStones = TestList
  [ TestCase $ stackStones ('a', 1) [Stone P1 F] (Cell ('a', 2) []) @?= Cell ('a', 2) []
  , TestCase $ stackStones ('a', 2) [Stone P1 F] (Cell ('a', 2) []) @?= Cell ('a', 2) [Stone P1 F]
  ]

testsUnstackStones = TestList
  [ TestCase $ unstackStones ('a', 1) 1 (Cell ('a', 2) [Stone P2 F, Stone P1 C]) @?= Cell ('a', 2) [Stone P2 F, Stone P1 C]
  , TestCase $ unstackStones ('a', 2) 1 (Cell ('a', 2) [Stone P2 F, Stone P1 C]) @?= Cell ('a', 2) [Stone P2 F]
  , TestCase $ unstackStones ('a', 2) 2 (Cell ('a', 2) [Stone P2 F, Stone P1 C]) @?= Cell ('a', 2) []
  ]

testsMoveSubstack = TestList
  [ TestCase $ moveSubstack mockBoard1 1 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P2 F, Stone P1 F]
    , Cell ('b', 2) [Stone P1 F]
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ moveSubstack mockBoard1 2 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P2 F, Stone P1 F, Stone P1 F]
    , Cell ('b', 2) []
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  ]

testsZipXYandCounts = TestList
  [ TestCase $ zipXYandCounts (1, ('b', 2), East, 1) @?= [(('b', 2), 1)]
  , TestCase $ zipXYandCounts (3, ('b', 2), East, 21) @?= [(('b', 2), 3), (('c', 2), 1)]
  , TestCase $ zipXYandCounts (3, ('b', 2), East, 111) @?= [(('b', 2), 3), (('c', 2), 2), (('d', 2), 1)]
  , TestCase $ zipXYandCounts (4, ('b', 2), East, 121) @?= [(('b', 2), 4), (('c', 2), 3), (('d', 2), 1)]
  ]

testsAction = TestList
  [ testsStackStones
  , testsUnstackStones
  , testsMoveSubstack
  , testsZipXYandCounts
  ]
