module CellSpec where

import Test.HUnit

import Tak
import Cell
import Display

mockStackS1 = [Stone P1 F, Stone P2 F, Stone P1 S]
mockStackC1 = [Stone P2 F, Stone P1 C]
mockStackF2 = [Stone P2 F]

mockCellA1 = Cell ('a', 1) mockStackS1
mockCellA2 = Cell ('a', 2) mockStackS1
mockCellA3 = Cell ('a', 3) mockStackF2
mockCellB3 = Cell ('b', 3) mockStackS1
mockCellC4 = Cell ('c', 4) mockStackC1

testsGetHeight = TestList
  [ TestCase $ getHeight mockCellB3 @?= 3
  , TestCase $ getHeight mockCellC4 @?= 2
  ]

testsGetOwner = TestList
  [ TestCase $ getOwner (Cell ('b', 2) []) @?= Nothing
  , TestCase $ getOwner (Cell ('b', 2) [Stone P1 F]) @?= Just P1
  , TestCase $ getOwner (Cell ('b', 2) [Stone P1 F, Stone P2 S]) @?= Just P2
  ]

testsGetTopStone = TestList
  [ TestCase $ getTopStone (Cell ('b', 2) []) @?= Nothing
  , TestCase $ getTopStone (Cell ('b', 2) [Stone P1 F]) @?= Just (Stone P1 F)
  , TestCase $ getTopStone (Cell ('b', 2) [Stone P1 F, Stone P2 C]) @?= Just (Stone P2 C)
  ]

testsHasCap = TestList
  [ TestCase $ hasCap (Cell ('a', 1) []) @?= False
  , TestCase $ hasCap (Cell ('a', 1) [Stone P1 S]) @?= False
  , TestCase $ hasCap (Cell ('a', 1) [Stone P1 C]) @?= True
  ]

testsIsEmpty = TestList
  [ TestCase $ isEmpty (Cell ('a', 1) []) @?= True
  , TestCase $ isEmpty (Cell ('a', 1) [Stone P1 F]) @?= False
  ]

testsIsToppable = TestList
  [ TestCase $ isToppable (Cell ('a', 1) []) @?= True
  , TestCase $ isToppable (Cell ('a', 1) [Stone P1 F]) @?= True
  , TestCase $ isToppable (Cell ('a', 1) [Stone P1 F, Stone P1 F]) @?= True
  , TestCase $ isToppable (Cell ('a', 1) [Stone P1 S]) @?= False
  , TestCase $ isToppable (Cell ('a', 1) [Stone P1 C]) @?= False
  ]

testsIsFlattenable = TestList
  [ TestCase $ isFlattenable (Cell ('a', 1) []) 1 @?= False
  , TestCase $ isFlattenable (Cell ('a', 1) [Stone P1 F]) 1 @?= False
  , TestCase $ isFlattenable (Cell ('a', 1) [Stone P1 C]) 1 @?= False
  , TestCase $ isFlattenable (Cell ('a', 1) [Stone P1 S]) 2 @?= False
  , TestCase $ isFlattenable (Cell ('a', 1) [Stone P1 S]) 1 @?= True
  , TestCase $ isFlattenable (Cell ('a', 1) [Stone P1 S]) 111 @?= True
  ]

testsCell = TestList
  [ testsGetHeight
  , testsGetTopStone
  , testsGetOwner
  , testsHasCap
  , testsIsEmpty
  , testsIsToppable
  , testsIsFlattenable
  ]
