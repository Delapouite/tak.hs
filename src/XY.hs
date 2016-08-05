module XY where

import Tak

-- coordinates in bounds?

isValidX :: Size -> X -> Bool
isValidX s x = x `elem` take s xs

isValidY :: Size -> Y -> Bool
isValidY s y = y > 0 && y <= s

isValidXY :: Size -> XY -> Bool
isValidXY s (x,y) = isValidX s x && isValidY s y

isValidSize :: Size -> Bool
isValidSize s = s >= minSize && s <= maxSize
