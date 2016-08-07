module MoveSpec where

import Test.HUnit

import Mock

import Tak
import Move

mockBoard1 =
  [ Cell ('a', 2) [Stone P2 F]
  , Cell ('b', 2) [Stone P1 S, Stone P2 F, Stone P1 F]
  , Cell ('a', 1) []
  , Cell ('b', 1) []
  ]

testsIsValidCount = TestList
  -- non existing cell
  [ TestCase $ isValidCount mockBoard (1, ('b', 4), North, []) @?= False
  -- count should not exceed board size
  , TestCase $ isValidCount mockBoard (5, ('a', 1), North, []) @?= False
  -- a,3 only has 1 Stone
  , TestCase $ isValidCount mockBoard (2, ('a', 3), North, []) @?= False
  , TestCase $ isValidCount mockBoard (2, ('a', 1), North, []) @?= True
  ]

testsIsValidDrops = TestList
  [ TestCase $ isValidDrops 3 [3] @?= True
  , TestCase $ isValidDrops 4 [3] @?= False
  , TestCase $ isValidDrops 4 [1, 2, 1] @?= True
  , TestCase $ isValidDrops 4 [1, 2, 2] @?= False
  ]

testsIsValidOwner = TestList
  [ TestCase $ isValidOwner mockBoard P1 ('a', 1) @?= True
  , TestCase $ isValidOwner mockBoard P2 ('a', 1) @?= False
  , TestCase $ isValidOwner mockBoard P2 ('b', 1) @?= False
  ]

testsMoveSubstack = TestList
  [ TestCase $ moveSubstack mockBoard1 1 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P1 S, Stone P2 F]
    , Cell ('b', 2) [Stone P2 F, Stone P1 F]
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ moveSubstack mockBoard1 2 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P1 S, Stone P2 F, Stone P2 F]
    , Cell ('b', 2) [Stone P1 F]
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ moveSubstack mockBoard1 3 ('b', 2) ('a', 2) @?=
    [ Cell ('a', 2) [Stone P1 S, Stone P2 F, Stone P1 F, Stone P2 F]
    , Cell ('b', 2) []
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  ]

testsZipXYandCounts = TestList
  [ TestCase $ zipXYandCounts mockSize (1, ('b', 2), East, [1]) @?= [(('b', 2), 1)]
  , TestCase $ zipXYandCounts mockSize (3, ('b', 2), East, [2, 1]) @?= [(('b', 2), 3), (('c', 2), 1)]
  , TestCase $ zipXYandCounts mockSize (3, ('b', 2), North, [2, 1]) @?= [(('b', 2), 3), (('b', 3), 1)]
  , TestCase $ zipXYandCounts mockSize (3, ('b', 2), East, [1, 1, 1]) @?= [(('b', 2), 3), (('c', 2), 2), (('d', 2), 1)]
  , TestCase $ zipXYandCounts mockSize (4, ('b', 2), East, [1, 2, 1]) @?= [(('b', 2), 4), (('c', 2), 3), (('d', 2), 1)]
  ]


testsMove = TestList
  [ testsIsValidCount
  , testsIsValidDrops
  , testsMoveSubstack
  , testsZipXYandCounts
  ]
