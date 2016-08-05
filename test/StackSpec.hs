module StackSpec where

import Test.HUnit

import Tak
import Stack

testsFlattenStack = TestList
  [ TestCase $ flattenStack [Stone P1 F, Stone P1 F] @?= [Stone P1 F, Stone P1 F]
  , TestCase $ flattenStack [Stone P1 S, Stone P2 F] @?= [Stone P1 F, Stone P2 F]
  ]

testsPushStones = TestList
  [ TestCase $ pushStones ('a', 1) [Stone P1 F] (Cell ('a', 2) []) @?= Cell ('a', 2) []
  , TestCase $ pushStones ('a', 2) [Stone P1 F] (Cell ('a', 2) []) @?= Cell ('a', 2) [Stone P1 F]
  , TestCase $ pushStones ('a', 2) [Stone P1 S, Stone P1 F] (Cell ('a', 2) []) @?= Cell ('a', 2) [Stone P1 S, Stone P1 F]
  ]

testsPopStones = TestList
  [ TestCase $ popStones ('a', 1) 1 (Cell ('a', 2) [Stone P1 C, Stone P2 F]) @?= Cell ('a', 2) [Stone P1 C, Stone P2 F]
  , TestCase $ popStones ('a', 2) 1 (Cell ('a', 2) [Stone P1 C, Stone P2 F]) @?= Cell ('a', 2) [Stone P2 F]
  , TestCase $ popStones ('a', 2) 2 (Cell ('a', 2) [Stone P1 C, Stone P2 F]) @?= Cell ('a', 2) []
  , TestCase $ popStones ('a', 2) 2 (Cell ('a', 2) [Stone P1 C, Stone P1 F, Stone P2 F]) @?= Cell ('a', 2) [Stone P2 F]
  ]

testsStack = TestList
  [ testsFlattenStack
  , testsPopStones
  , testsPushStones
  ]
