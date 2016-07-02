module Command where

import Tak
import Board
import Conversion
import Display
import Game
import Parser
import Validation

handleShow :: Game -> Either X Y -> (Game, Display)
handleShow g xory = case xory of
  Left x -> if isValidX s x then (g, showCol b x) else (g, "Wrong x coordinate")
  Right y -> if isValidY s y then (g, showRow b y) else (g, "Wrong y coordinate")
  where
    s = size g
    b = board g

handlePlace :: Game -> XY -> StoneType -> (Game, Display)
handlePlace g xy st
  | not $ isValidXY (size g) xy   = (g, "Wrong xy coordinates")
  | not $ canPlace b xy           = (g, "The cell must be empty")
  | st == C && not (capsInDeck g) = (g, "No more caps in deck")
  | otherwise                     = updateAndShowGame g $ placeStone b xy (getPlayer g) st
  where
    b = board g

handleMove :: Game -> Move -> (Game, Display)
handleMove g m@(count, xy, dir, drops)
  | not $ isValidXY (size g) xy    = (g, "Wrong xy coordinates")
  | not $ isUnderControl g xy      = (g, "You do not control the cell")
  | not $ isValidCount g m         = (g, "Wrong count")
  | not $ isValidDrops count drops = (g, "Wrong drops")
  | not $ isDropzoneClear g m      = (g, "The dropzone is not clear")
  | otherwise                      = updateAndShowGame g $ moveStack (board g) m

handleCommand :: Game -> Command -> (Game, Display)
handleCommand g a = case a of
    (Command "show" (coord:_)) -> handleShow g $ toXorY coord
    (Command "show" _) -> (g, showGame g)
    (Command "place" (args:_)) -> case parsePlace args of
      Just (sType, xy) -> handlePlace g xy sType
      _ -> (g, "Wrong stone type or xy coordinates")
    (Command "move" (args:_)) -> case parseMove args of
      Just m -> handleMove g m
      _ -> (g, "Wrong args for move")
    _ -> (g, "Unknown command")
