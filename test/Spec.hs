import Test.HUnit

import ActionSpec
import CellSpec
import ConversionSpec
import ParserSpec
import TPSSpec
import ValidationSpec

main :: IO Counts
main = runTestTT $ TestList
  [ testsAction
  , testsCell
  , testsConversion
  , testsParser
  , testsTPS
  , testsValidation
  ]
