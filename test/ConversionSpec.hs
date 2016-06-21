module ConversionSpec where

import Test.HUnit

import Tak
import Conversion

testGetSize = TestCase $ assertEqual "Should return side length of the Board" exp act
  where
    exp = 5
    act = getSize $ initBoard exp

testsConversion = TestList
  [ testGetSize ]
