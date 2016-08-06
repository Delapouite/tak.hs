module CellSpec where

import Test.HUnit

import Tak
import Cell
import Display

mockXY = ('a', 1)

-- associated player

testsGetOwner = TestList
  [ TestCase $ getOwner (Cell mockXY []) @?= Nothing
  , TestCase $ getOwner (Cell mockXY [Stone P1 F]) @?= Just P1
  , TestCase $ getOwner (Cell mockXY [Stone P2 S, Stone P1 F]) @?= Just P2
  ]

testsIsOwnedBy = TestList
  [ TestCase $ isOwnedBy P1 (Cell mockXY []) @?= False
  , TestCase $ isOwnedBy P1 (Cell mockXY [Stone P1 F]) @?= True
  , TestCase $ isOwnedBy P2 (Cell mockXY [Stone P2 S, Stone P1 F]) @?= True
  , TestCase $ isOwnedBy P1 (Cell mockXY [Stone P2 S, Stone P1 F]) @?= False
  ]

-- associated stack

testsGetHeight = TestList
  [ TestCase $ getHeight (Cell mockXY []) @?= 0
  , TestCase $ getHeight (Cell mockXY [Stone P1 F, Stone P2 F]) @?= 2
  , TestCase $ getHeight (Cell mockXY [Stone P1 C, Stone P1 F, Stone P2 F]) @?= 3
  ]

testsGetTopStone = TestList
  [ TestCase $ getTopStone (Cell mockXY []) @?= Nothing
  , TestCase $ getTopStone (Cell mockXY [Stone P1 F]) @?= Just (Stone P1 F)
  , TestCase $ getTopStone (Cell mockXY [Stone P2 C, Stone P1 F]) @?= Just (Stone P2 C)
  ]

testsIsEmpty = TestList
  [ TestCase $ isEmpty (Cell mockXY []) @?= True
  , TestCase $ isEmpty (Cell mockXY [Stone P1 F]) @?= False
  ]

testsIsCapped = TestList
  [ TestCase $ isCapped (Cell mockXY []) @?= False
  , TestCase $ isCapped (Cell mockXY [Stone P1 S]) @?= False
  , TestCase $ isCapped (Cell mockXY [Stone P1 C]) @?= True
  ]

testsIsToppable = TestList
  [ TestCase $ isToppable (Cell mockXY []) @?= True
  , TestCase $ isToppable (Cell mockXY [Stone P1 F]) @?= True
  , TestCase $ isToppable (Cell mockXY [Stone P1 F, Stone P1 F]) @?= True
  , TestCase $ isToppable (Cell mockXY [Stone P1 S, Stone P1 F]) @?= False
  , TestCase $ isToppable (Cell mockXY [Stone P1 S]) @?= False
  , TestCase $ isToppable (Cell mockXY [Stone P1 C]) @?= False
  ]

testsIsFlattenable = TestList
  [ TestCase $ isFlattenable (Cell mockXY []) [1] @?= False
  , TestCase $ isFlattenable (Cell mockXY [Stone P1 F]) [1] @?= False
  , TestCase $ isFlattenable (Cell mockXY [Stone P1 C]) [1] @?= False
  , TestCase $ isFlattenable (Cell mockXY [Stone P1 S]) [2] @?= False
  , TestCase $ isFlattenable (Cell mockXY [Stone P1 S]) [1] @?= True
  , TestCase $ isFlattenable (Cell mockXY [Stone P1 S]) [1, 1, 1] @?= True
  ]

testsCell = TestList
  [ testsGetOwner
  , testsIsOwnedBy
  , testsGetHeight
  , testsGetTopStone
  , testsIsEmpty
  , testsIsCapped
  , testsIsToppable
  , testsIsFlattenable
  ]
