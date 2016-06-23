module ParserSpec where

import Test.HUnit

import Tak
import Display
import Parser

testsParseMove = TestList
  [ TestCase $ parseMove "b2" @?= Nothing
  , TestCase $ parseMove "2b" @?= Nothing
  , TestCase $ parseMove "2bb" @?= Nothing
  , TestCase $ parseMove "2b1" @?= Nothing
  , TestCase $ parseMove "2b1!" @?= Nothing

  , TestCase $ parseMove "b1+" @?= Just (1, ('b', 1), North, 1)
  , TestCase $ parseMove "2b1>" @?= Just (2, ('b', 1), East, 2)
  , TestCase $ parseMove "2b1>11" @?= Just (2, ('b', 1), East, 11)
  ]

testsParseXY = TestList
  [ TestCase $ parseXY "b3" @?= Just ('b', 3)
  , TestCase $ parseXY "B3" @?= Just ('b', 3)
  , TestCase $ parseXY "3b" @?= Nothing
  ]

testsParseCount = TestList
  [ TestCase $ parseCount "b3>" @?= (1, "b3>")
  , TestCase $ parseCount "4b3>" @?= (4, "b3>")
  ]

testsParseDir = TestList
  [ TestCase $ parseDir '+' @?= Just North
  , TestCase $ parseDir '0' @?= Nothing
  ]

testsParseDrops = TestList
  [ TestCase $ parseDrops 3 "121" @?= 121
  , TestCase $ parseDrops 3 "" @?= 3
  ]

testsParseTPSBoard = TestList
  [ TestCase $ parseTPSBoard "x2/x2" @?=
    [ Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('a', 1) []
    , Cell ('b', 1) []
    ]
  , TestCase $ parseTPSBoard "1,x,21S/x2,211/1,2C,2" @?=
    [ Cell ('a', 3) [Stone P1 F]
    , Cell ('b', 3) []
    , Cell ('c', 3) [Stone P2 F, Stone P1 S]
    , Cell ('a', 2) []
    , Cell ('b', 2) []
    , Cell ('c', 2) [Stone P2 F, Stone P1 F, Stone P1 F]
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
    [ Cell ('a', 4) [Stone P1 F, Stone P2 S]
    , Cell ('b', 4) [Stone P2 F]
    , Cell ('c', 4) []
    , Cell ('d', 4) []
    ]
  ]

testsParseTPSX = TestList
  [ TestCase $ parseTPSX "x3" @?= "x,x,x"
  , TestCase $ parseTPSX "x" @?= "x"
  , TestCase $ parseTPSX "12S" @?= "12S"
  ]

testsParseTPSCell = TestList
  [ TestCase $ parseTPSCell "2" ('b', 3) @?= Cell ('b', 3) [Stone P2 F]
  , TestCase $ parseTPSCell "2C" ('b', 3) @?= Cell ('b', 3) [Stone P2 C]
  ]

testsParseTPSStack = TestList
  [ TestCase $ parseTPSStack "x" @?= []
  , TestCase $ parseTPSStack "2" @?= [Stone P2 F]
  , TestCase $ parseTPSStack "12" @?= [Stone P1 F, Stone P2 F]
  , TestCase $ parseTPSStack "12S" @?= [Stone P1 F, Stone P2 S]
  , TestCase $ parseTPSStack "21C" @?= [Stone P2 F, Stone P1 C]
  ]

testsParser = TestList
  [ testsParseXY
  , testsParseMove
  , testsParseCount
  , testsParseDir
  , testsParseDrops
  , testsParseTPSBoard
  , testsParseTPSRow
  , testsParseTPSX
  , testsParseTPSCell
  , testsParseTPSStack
  ]
