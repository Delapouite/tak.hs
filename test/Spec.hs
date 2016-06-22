import Test.HUnit

import ConversionSpec
import ParserSpec
import ValidationSpec

main :: IO Counts
main = runTestTT $ TestList
  [ testsConversion
  , testsParser
  , testsValidation
  ]
