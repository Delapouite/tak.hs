module TPSSpec where

import Test.HUnit

import Tak
import Display
import TPS

testsParseTPSBoard = TestList
  [ TestCase $ parseTPSBoard "x2/x2" @?=
    [ Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ parseTPSBoard "x2/x2/1,x" @?=
    [ Cell ('a', 3) []
    , Cell ('b', 3) []
    , Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('a', 1) [Stone P1 F]
    , Cell ('b', 1) []
    ]
  , TestCase $ parseTPSBoard "1,x,21S/x2,211/1,2C,2" @?=
    [ Cell ('a', 3) [Stone P1 F]
    , Cell ('b', 3) []
    , Cell ('c', 3) [Stone P1 S, Stone P2 F]
    , Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('c', 2) [Stone P1 F, Stone P1 F, Stone P2 F]
    , Cell ('a', 1) [Stone P1 F]
    , Cell ('b', 1) [Stone P2 C]
    , Cell ('c', 1) [Stone P2 F]
    ]
  ]

testsToTPSBoard = TestList
  [ TestCase $ "x2/x2" @=? toTPSBoard
    [ Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ "x2/x2/1,x" @=? toTPSBoard
    [ Cell ('a', 3) []
    , Cell ('b', 3) []
    , Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('a', 1) [Stone P1 F]
    , Cell ('b', 1) []
    ]
  , TestCase $ "1,x,21S/x2,211/1,2C,2" @=? toTPSBoard
    [ Cell ('a', 3) [Stone P1 F]
    , Cell ('b', 3) []
    , Cell ('c', 3) [Stone P1 S, Stone P2 F]
    , Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('c', 2) [Stone P1 F, Stone P1 F, Stone P2 F]
    , Cell ('a', 1) [Stone P1 F]
    , Cell ('b', 1) [Stone P2 C]
    , Cell ('c', 1) [Stone P2 F]
    ]
  ]

testsParseTPSRow = TestList
  [ TestCase $ parseTPSRow "x3" 2 @?=
    [ Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('c', 2) []
    ]
  , TestCase $ parseTPSRow "x,2,x" 1 @?=
    [ Cell ('a', 1) []
    , Cell ('b', 1) [Stone P2 F]
    , Cell ('c', 1) []
    ]
  , TestCase $ parseTPSRow "12S,2,x2" 4 @?=
    [ Cell ('a', 4) [Stone P2 S, Stone P1 F]
    , Cell ('b', 4) [Stone P2 F]
    , Cell ('c', 4) []
    , Cell ('d', 4) []
    ]
  , TestCase $ parseTPSRow "12S,x2,2" 4 @?=
    [ Cell ('a', 4) [Stone P2 S, Stone P1 F]
    , Cell ('b', 4) []
    , Cell ('c', 4) []
    , Cell ('d', 4) [Stone P2 F]
    ]
  ]

testsToTPSRow = TestList
  [ TestCase $ "x3" @=? toTPSRow
    [ Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('c', 2) []
    ]
  , TestCase $ "x,2,x" @=? toTPSRow
    [ Cell ('a', 1) []
    , Cell ('b', 1) [Stone P2 F]
    , Cell ('c', 1) []
    ]
  , TestCase $ "12S,2,x2" @=? toTPSRow
    [ Cell ('a', 4) [Stone P2 S, Stone P1 F]
    , Cell ('b', 4) [Stone P2 F]
    , Cell ('c', 4) []
    , Cell ('d', 4) []
    ]
  , TestCase $ "12S,x2,2" @=? toTPSRow
    [ Cell ('a', 4) [Stone P2 S, Stone P1 F]
    , Cell ('b', 4) []
    , Cell ('c', 4) []
    , Cell ('b', 4) [Stone P2 F]
    ]
  ]

testsParseTPSX = TestList
  [ TestCase $ parseTPSX "x3" @?= "x,x,x"
  , TestCase $ parseTPSX "x" @?= "x"
  , TestCase $ parseTPSX "12S" @?= "12S"
  ]

testsToTPSX = TestList
  [ TestCase $ toTPSX "xxx" @?= "x3"
  , TestCase $ toTPSX "x" @?= "x"
  , TestCase $ toTPSX "12S" @?= "12S"
  ]

testsParseTPSCell = TestList
  [ TestCase $ parseTPSCell "2" ('b', 3) @?= Cell ('b', 3) [Stone P2 F]
  , TestCase $ parseTPSCell "2C" ('b', 3) @?= Cell ('b', 3) [Stone P2 C]
  , TestCase $ parseTPSCell "12C" ('b', 3) @?= Cell ('b', 3) [Stone P2 C, Stone P1 F]
  ]

testsToTPSCell = TestList
  [ TestCase $ toTPSCell (Cell ('b', 3) []) @?= "x"
  , TestCase $ toTPSCell (Cell ('b', 3) [Stone P2 F]) @?= "2"
  , TestCase $ toTPSCell (Cell ('b', 3) [Stone P2 C]) @?= "2C"
  , TestCase $ toTPSCell (Cell ('b', 3) [Stone P2 C, Stone P1 F]) @?= "12C"
  ]

testsParseTPSStack = TestList
  [ TestCase $ parseTPSStack "x" @?= []
  , TestCase $ parseTPSStack "2" @?= [Stone P2 F]
  , TestCase $ parseTPSStack "12" @?= [Stone P2 F, Stone P1 F]
  , TestCase $ parseTPSStack "12S" @?= [Stone P2 S, Stone P1 F]
  , TestCase $ parseTPSStack "21C" @?= [Stone P1 C, Stone P2 F]
  , TestCase $ parseTPSStack "121C" @?= [Stone P1 C, Stone P2 F, Stone P1 F]
  ]

testsToTPSStack = TestList
  [ TestCase $ toTPSStack [] @?= "x"
  , TestCase $ toTPSStack [Stone P2 F] @?= "2"
  , TestCase $ toTPSStack [Stone P2 F, Stone P1 F] @?= "12"
  , TestCase $ toTPSStack [Stone P2 S, Stone P1 F] @?= "12S"
  , TestCase $ toTPSStack [Stone P1 C, Stone P2 F] @?= "21C"
  , TestCase $ toTPSStack [Stone P1 C, Stone P2 F, Stone P1 F] @?= "121C"
  ]

testsToTPSStone = TestList
  [ TestCase $ toTPSStone (Stone P1 F) @?= "1"
  , TestCase $ toTPSStone (Stone P2 F) @?= "2"
  , TestCase $ toTPSStone (Stone P1 S) @?= "1S"
  , TestCase $ toTPSStone (Stone P2 C) @?= "2C"
  ]

testsToTPSPlayer = TestList
  [ TestCase $ toTPSPlayer P1 @?= "1"
  , TestCase $ toTPSPlayer P2 @?= "2"
  ]

testsTPS = TestList
  [ testsParseTPSBoard
  , testsToTPSBoard
  , testsParseTPSRow
  , testsToTPSRow
  , testsParseTPSX
  , testsToTPSX
  , testsParseTPSCell
  , testsToTPSCell
  , testsParseTPSStack
  , testsToTPSStack
  , testsToTPSStone
  , testsToTPSPlayer
  ]
