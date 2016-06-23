import Test.HUnit

import ActionSpec
import ConversionSpec
import ParserSpec
import ValidationSpec

main :: IO Counts
main = runTestTT $ TestList
  [ testsAction
  , testsConversion
  , testsParser
  , testsValidation
  ]
