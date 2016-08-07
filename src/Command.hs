module Command where

import Control.Monad.Reader

import Tak
import Conversion
import Display
import Game
import Move
import Parser
import Place
import XY

handleShow :: Game -> Either X Y -> (Game, Display)
handleShow g@Game {board = b, options = o, size = s} xory = case xory of
  Left x -> if isValidX s x
    then (g, runReader (showCol b x) o)
    else (g, "Wrong x coordinate")
  Right y -> if isValidY s y
    then (g, runReader (showRow b y) o)
    else (g, "Wrong y coordinate")

handlePlace :: Game -> XY -> StoneType -> (Game, Display)
handlePlace g@Game {board = b, size = s} xy st
  | not $ isValidXY s xy            = (g, "Wrong xy coordinates")
  | not $ canPlace b xy             = (g, "The cell must be empty")
  | st == C && not (capsInDeck b p) = (g, "No more caps in deck")
  | otherwise                       = updateAndShowGame g $ placeStone b xy p st
  where
    p = getPlayer g

handleMove :: Game -> Move -> (Game, Display)
handleMove g@Game {board = b, player = p} m@(count, xy, dir, drops)
  | not $ isValidXY (size g) xy    = (g, "Wrong xy coordinates")
  | not $ isValidOwner b p xy      = (g, "You do not own the cell")
  | not $ isValidCount b m         = (g, "Wrong count")
  | not $ isValidDrops count drops = (g, "Wrong drops")
  | not $ isDropzoneClear b m      = (g, "The dropzone is not clear")
  | otherwise                      = updateAndShowGame g $ moveStack b m

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

  -- shortcuts
  (Command verb _) -> case parseUnknownVerb verb of
    Just c -> handleCommand g c
    _ -> (g, "Unknown command")

