import Test.HUnit

import ActionSpec
import BoardSpec
import CellSpec
import ConversionSpec
import ParserSpec
import TPSSpec
import ValidationSpec

main :: IO Counts
main = runTestTT $ TestList
  [ testsAction
  , testsBoard
  , testsCell
  , testsConversion
  , testsParser
  , testsTPS
  , testsValidation
  ]
