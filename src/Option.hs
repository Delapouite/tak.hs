module Option where

import Tak

type Colored = Bool

inColors :: Game -> Bool
inColors = optColors . options

