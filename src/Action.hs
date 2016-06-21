module Action where

import Data.Char (digitToInt)

import Tak
import Conversion
import Display
import Parser
import Validation

placeStone :: Board -> XY -> Stone -> Board
placeStone b xy stone = map (stackStones xy [stone]) b

stackStones :: XY -> [Stone] -> Cell -> Cell
stackStones xy stones c@(Cell xy' zs) = if xy == xy'
  then Cell xy (flattenStack zs ++ stones)
  else c

unstackStones :: XY -> Count -> Cell -> Cell
unstackStones xy count c@(Cell xy' zs) = if xy == xy'
  then Cell xy (drop count $ reverse zs)
  else c

placeStoneInGame :: Game -> XY -> StoneType -> (Game, Display)
placeStoneInGame g xy st = (g', showBoardWithAxes b')
  where
    b = board g
    b' = placeStone b xy (Stone (getPlayer g) st)
    g' = g { board = b', turn = turn g + 1 }

-- turn all stones to F
flattenStack :: Stack -> Stack
flattenStack = map (\(Stone p t) -> (Stone p F))

moveSubstack :: Board -> Count -> XY -> XY -> Board
moveSubstack b count fromXY toXY = map (stackStones toXY stones) b'
  where
    Just (Cell _ zs) = getCell b fromXY
    stones = take count $ reverse zs
    b' = map (unstackStones fromXY count) b

-- TODO
moveStack :: Game -> Move -> (Game, Display)
moveStack g m@(count, xy, dir, drops) = (g', showBoardWithAxes b')
  where
    b = board g
    reducer acc (xy, drop) = moveSubstack acc drop xy (getNextXY xy dir)
    b' = foldl reducer b $ zipXYandDrops m
    g' = g { board = b', turn = turn g + 1 }

zipXYandDrops :: Move -> [(XY, Int)]
zipXYandDrops m@(count, xy, dir, drops) = zip xys drops'
  where
    xys = getNextXYs xy dir drops
    drops' = map digitToInt (show drops)

-- handlers

handleShow :: Game -> Either X Y -> (Game, Display)
handleShow g xory = case xory of
    Left x -> if isValidX g x then (g, showCol b x) else (g, "Wrong x coordinate")
    Right y -> if isValidY g y then (g, showRow b y) else (g, "Wrong y coordinate")
  where
    b = board g

handlePlace :: Game -> XY -> StoneType -> (Game, Display)
handlePlace g xy st
  | not $ isValidXY g xy          = (g, "Wrong xy coordinates")
  | not $ canStack g xy           = (g, "The cell must be empty")
  | st == C && not (capsInDeck g) = (g, "No more caps in deck")
  | otherwise                     = placeStoneInGame g xy st

handleMove :: Game -> Move -> (Game, Display)
handleMove g m@(count, xy, dir, drops)
  | not $ isValidXY g xy           = (g, "Wrong xy coordinates")
  | not $ isUnderControl g m       = (g, "You do not control the cell")
  | not $ isValidCount g m         = (g, "Wrong count")
  | not $ isValidDrops count drops = (g, "Wrong drops")
  | not $ isDropzoneClear g m      = (g, "The dropzone is not clear")
  | otherwise                      = moveStack g m

handleAction :: Game -> Action -> (Game, Display)
handleAction g a = case a of
    (Action "show" (coord:_)) -> handleShow g $ toXorY coord
    (Action "show" _) -> (g, showGame g)
    (Action "place" (args:_)) -> case parsePlace args of
      Just (sType, xy) -> handlePlace g xy sType
      _ -> (g, "Wrong stone type or xy coordinates")
    (Action "move" (args:_)) -> case parseMove args of
      Just m -> handleMove g m
      _ -> (g, "Wrong args for move")
    _ -> (g, "Unknown action")

