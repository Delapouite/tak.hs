module MoveSpec where

import Test.HUnit

import Mock

import Tak
import Move

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

testsMove = TestList
  [ testsIsValidCount
  , testsIsValidDrops
  ]
