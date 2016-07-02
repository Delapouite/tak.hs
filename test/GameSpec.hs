module GameSpec where

import Test.HUnit

import Tak
import Game


testsGetPlayer = TestList
  [ TestCase $ getPlayer Game {board = [], player = P1, size = 0, turn = 1} @?= P2
  , TestCase $ getPlayer Game {board = [], player = P2, size = 0, turn = 1} @?= P1
  , TestCase $ getPlayer Game {board = [], player = P1, size = 0, turn = 3} @?= P1
  ]

testsGame = TestList
  [ testsGetPlayer
  ]

