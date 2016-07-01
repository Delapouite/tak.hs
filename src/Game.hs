module Game where

import Tak

-- on turn 1, players place an opponent's stone
getPlayer :: Game -> Player
getPlayer g
  | turn g /= 1 = p
  | p == P1     = P2
  | otherwise   = P1
  where
    p = player g

