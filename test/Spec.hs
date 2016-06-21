import Test.HUnit
import ConversionSpec

main :: IO Counts
main = runTestTT $ TestList [ testGetSize ]
