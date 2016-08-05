module XY where

import Tak

-- coordinates

isValidX :: Size -> X -> Bool
isValidX s x = x `elem` take s xs

isValidY :: Size -> Y -> Bool
isValidY s y = y > 0 && y <= s

-- in bounds
isValidXY :: Size -> XY -> Bool
isValidXY s (x,y) = isValidX s x && isValidY s y

isValidSize :: Size -> Bool
isValidSize s = s >= minSize && s <= maxSize
