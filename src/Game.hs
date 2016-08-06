module Game where

import Control.Monad.Reader

import Tak
import Display

-- on turn 1, players place an opponent's stone
getPlayer :: Game -> Player
getPlayer Game {player = p, turn = t}
  | t /= 1 = p
  | p == P1     = P2
  | otherwise   = P1

updateGame :: Game -> Board -> Game
updateGame g b = let
  p = if player g == P1 then P2 else P1
  -- new turn?
  t = if p == P1 then turn g + 1 else turn g
  in g { board = b, player = p, turn = t }

updateAndShowGame :: Game -> Board -> (Game, Display)
updateAndShowGame g b = (updateGame g b, runReader (showBoardWithAxes b) (options g))
