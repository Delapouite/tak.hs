module ConversionSpec where

import Test.HUnit

import Tak
import Conversion

testGetSize :: Test
testGetSize =
  TestCase $ assertEqual "Should return side length of the Board"
    s s'
    where
      s = 5
      s' = getSize $ initBoard s
