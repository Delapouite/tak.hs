module MoveSpec where

import Test.HUnit

import Tak
import Move

testsIsValidDrops = TestList
  [ TestCase $ isValidDrops 3 [3] @?= True
  , TestCase $ isValidDrops 4 [3] @?= False
  , TestCase $ isValidDrops 4 [1, 2, 1] @?= True
  , TestCase $ isValidDrops 4 [1, 2, 2] @?= False
  ]

testsMove = TestList
  [ testsIsValidDrops
  ]
