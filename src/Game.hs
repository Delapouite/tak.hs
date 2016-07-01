module Game where

import Tak
import Display

-- on turn 1, players place an opponent's stone
getPlayer :: Game -> Player
getPlayer g
  | turn g /= 1 = p
  | p == P1     = P2
  | otherwise   = P1
  where
    p = player g

updateGame :: Game -> Board -> Game
updateGame g b = g { board = b, player = p, turn = t }
  where
    p = if player g == P1 then P2 else P1
    -- new turn?
    t = if p == P1 then turn g + 1 else turn g

updateAndShowGame :: Game -> Board -> (Game, Display)
updateAndShowGame g b = (updateGame g b, showBoardWithAxes b)

