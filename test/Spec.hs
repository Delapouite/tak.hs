import Test.HUnit

import ConversionSpec
import ParserSpec

main :: IO Counts
main = runTestTT $ TestList
  [ testsConversion
  , testsParser
  ]
