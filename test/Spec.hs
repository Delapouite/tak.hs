import Test.HUnit

import BoardSpec
import CellSpec
import CommandSpec
import ConversionSpec
import DisplaySpec
import GameSpec
import ParserSpec
import StackSpec
import TPSSpec
import ValidationSpec
import XYSpec

main :: IO Counts
main = runTestTT $ TestList
  [ testsBoard
  , testsCell
  , testsCommand
  , testsConversion
  , testsDisplay
  , testsGame
  , testsParser
  , testsStack
  , testsTPS
  , testsValidation
  , testsXY
  ]
