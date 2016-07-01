import Test.HUnit

import BoardSpec
import CellSpec
import CommandSpec
import ConversionSpec
import ParserSpec
import TPSSpec
import ValidationSpec

main :: IO Counts
main = runTestTT $ TestList
  [ testsBoard
  , testsCell
  , testsCommand
  , testsConversion
  , testsParser
  , testsTPS
  , testsValidation
  ]
