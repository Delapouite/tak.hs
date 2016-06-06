-- https://www.youtube.com/watch?v=iEXkpS-Q9dI

-- Coordinates system according to Portable Tak Notation
-- 3
-- 2
-- 1
--  a b c

-- PTN
-- Place flat stone at a1: a1
-- Place capstone at b4: Cb4
-- Place standing stone at d3: Sd3

import Data.Char (digitToInt, isAlpha, isDigit, ord, toLower, toUpper)
import Data.List (find, transpose)
import Data.List.Split (chunksOf)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- types

data Player = P1 | P2 deriving (Eq, Show)

-- Flat, Standing or Cap
data StoneType = F | S | C deriving (Eq, Show, Read)

-- P1 in UpperCase, P2 in LowerCase
data Stone = Stone Player StoneType
instance Show Stone where
  show (Stone P1 F) = "F"
  show (Stone P2 F) = "f"
  show (Stone P1 S) = "S"
  show (Stone P2 S) = "s"
  show (Stone P1 C) = "C"
  show (Stone P2 C) = "c"

-- bottom to top (zs)
type Stack = [Stone]

data Cell = Cell Char Int Stack
instance Show Cell where
  show (Cell _ _ zs) = showStack zs

type Board = [Cell]
type Col = [Cell]
type Row = [Cell]
type X = Char
type Y = Int
type XY = (X, Y)
type Move = (Int, XY, Dir, Int)

-- PTN: + > - <
data Dir = North | East | South | West deriving (Show)

type Verb = String
data Action = Action Verb [String]
type Display = String

data Game = Game { size :: Int
                 , board :: Board
                 , turn :: Int
                 }

minSize = 3
maxSize = 8

-- axis
xs = ['a'..]

initBoard :: Int -> Board
initBoard size = take (size ^ 2) [Cell x y [] | x <- xs, y <- [1..size]]

stoneCount :: Int -> Int
stoneCount 3 = 10
stoneCount 4 = 15
stoneCount 5 = 21
stoneCount 6 = 30
stoneCount 7 = 40
stoneCount 8 = 50

capCount :: Int -> Int
capCount 8 = 2
capCount 4 = 0
capCount 3 = 0
capCount _ = 1

-- inspection

getSize :: Board -> Int
getSize = truncate . sqrt . fromIntegral . length

getPlayer :: Game -> Player
getPlayer g = if turn g `mod` 2 == 0 then P2 else P1

getPlacedByPlayer :: Game -> Player -> [Stone]
getPlacedByPlayer g p = concatMap getOwnStones $ getStacks g
  where
    getOwnStones = filter (\(Stone owner _) -> owner == p)

getPlaced :: Game -> [Stone]
getPlaced g = getPlacedByPlayer g P1 ++ getPlacedByPlayer g P2

getPlacedByPlayerAndType :: Game -> Player -> StoneType -> [Stone]
getPlacedByPlayerAndType g p st = filter (\(Stone _ t) -> t == st) $ getPlacedByPlayer g p

getCell :: Board -> XY -> Maybe Cell
getCell b (x,y) = find (\(Cell cx cy _) -> cx == x && cy == y) b

getStacks :: Game -> [Stack]
getStacks g = map (\(Cell _ _ zs) -> zs) $ board g

getStackHeight :: Cell -> Int
getStackHeight (Cell _ _ zs) = length zs

getTallerStackHeight :: [Cell] -> Int
getTallerStackHeight cells = maximum $ map getStackHeight cells

-- TODO maybeLast?
getTopStone :: Cell -> Maybe Stone
getTopStone (Cell _ _ zs)
  | null zs   = Nothing
  | otherwise = Just $ last zs

-- validation

isValidX :: Game -> X -> Bool
isValidX g x = x' >= 0 && x' <= size g
  where
    x' = xToInt x

isValidY :: Game -> Y -> Bool
isValidY g y = y > 0 && y <= size g

isValidXY :: Game -> XY -> Bool
isValidXY g (x,y) = isValidX g x && isValidY g y

isValidSize :: Int -> Bool
isValidSize s = s >= minSize && s <= maxSize

isValidMove :: Game -> Move -> Bool
isValidMove g m@(count, xy, dir, drops) =
  isValidXY g xy && isValidCount g m && isValidDrops count drops

-- carry limit, stack height
isValidCount :: Game -> Move -> Bool
isValidCount g (count, xy, _, _) =
  count <= getSize b && validStackHeight
  where
    b = board g
    validStackHeight = case getCell b xy of
      Just c -> getStackHeight c <= count
      Nothing -> False

isValidDrops :: Int -> Int -> Bool
isValidDrops c d = c == d || c == (sum . map digitToInt . show) d

-- only in empty cells
canStack :: Game -> XY -> Bool
canStack g xy = case getCell (board g) xy of
  Just (Cell _ _ zs) -> null zs
  Nothing -> False

capsInDeck :: Game -> Bool
capsInDeck g = totalCaps - placedCaps > 0
  where
    p = getPlayer g
    totalCaps = capCount $ size g
    placedCaps = length $ getPlacedByPlayerAndType g p C

isBoardFull :: Board -> Bool
isBoardFull = not . any (\(Cell _ _ zs) -> null zs)

checkEnd :: Game -> Maybe Display
checkEnd g = if isBoardFull $ board g
  then Just ("Board's full! Flat winner is " ++ show winner)
  else Nothing
  where
    p1FlatsCount = length $ getPlacedByPlayerAndType g P1 F
    p2FlatsCount = length $ getPlacedByPlayerAndType g P2 F
    winner = if p1FlatsCount > p2FlatsCount then P1 else P2

isUnderControl :: Game -> Move -> Bool
isUnderControl g (_, xy, _, _) = case getCell (board g) xy of
  Nothing -> False
  Just c -> case getTopStone c of
    Nothing -> False
    Just (Stone owner _) -> owner == getPlayer g

-- TODO
isDropzoneClear :: Game -> Move -> Bool
isDropzoneClear g m = True

-- conversion

-- 97 = ASCII 'a'
xToInt :: X -> Int
xToInt x = ord x - 97

toXorY :: String -> Either X Y
toXorY arg = if isDigit $ head arg
  then Right (read [head arg] :: Int)
  else Left  (head arg)

toStoneType :: String -> StoneType
toStoneType arg = read (map toUpper arg) :: StoneType

toCols :: Board -> [Col]
toCols b = chunksOf (getSize b) b

toRows :: Board -> [Row]
toRows = transpose . toCols

-- display

showBoard :: Board -> Display
showBoard = unlines . reverse . map showCells . toRows

showCells :: [Cell] -> Display
showCells = unwords . map show

showStack :: Stack -> Display
showStack [] = "."
showStack zs = show $ last zs

showStackLevel :: [Cell] -> Int -> Display
showStackLevel cs i = unwords stones
  where
    stones = map getStone cs
    getStone (Cell _ _ zs) = if not (null zs) && (length zs - 1 >= i)
      then show $ zs !! i
      else " "

showStacks :: [Cell] -> Display
showStacks cs = unlines $ map (showStackLevel cs) levels
  where
    levels = reverse [0..getTallerStackHeight cs - 1]

showColByX :: Board -> X -> Display
showColByX b x = col ++ showYAxis b
  where
    cols = toCols b
    col = showStacks . reverse $ cols !! xToInt x

showRowByY :: Board -> Y -> Display
showRowByY b y = row ++ showXAxis b
  where
    rows = toRows b
    row = showStacks $ rows !! (y - 1)

showBoardWithYAxis :: Board -> Display
showBoardWithYAxis = unlines . reverse . map showRowWithY . toRows

showBoardWithAxes :: Board -> Display
showBoardWithAxes b = "\n" ++ showBoardWithYAxis b ++ "  " ++ showXAxis b

showRowWithY :: Row -> Display
showRowWithY r = show y ++ " " ++ showCells r
  where
    (Cell _ y _) = head r

-- horizontally
showYAxis :: Board -> Display
showYAxis b = unwords $ map show $ reverse $ take (getSize b) [1..]

showXAxis :: Board -> Display
showXAxis b = unwords $ map (: []) $ take (getSize b) xs

showDeck :: Game -> Player -> Display
showDeck g p = show p ++ "'s deck: " ++ flats ++ " " ++ caps ++ "\n"
  where
    total = stoneCount $ size g
    totalCaps = capCount $ size g
    placedCaps = length $ getPlacedByPlayerAndType g p C
    placed = length (getPlacedByPlayer g p) - placedCaps
    flats = show (total - placed)
    caps = show (totalCaps - placedCaps) ++ show (Stone p C)

showDecks :: Game -> Display
showDecks g = "\n" ++ showDeck g P1 ++ showDeck g P2

showGame :: Game -> Display
showGame g = showDecks g ++ showBoardWithAxes (board g)

-- parsers

parseAction :: String -> Action
parseAction s = Action verb args
  where
    (verb:args) = words s

parseXY :: String -> Maybe XY
parseXY (x:y:[]) = if isAlpha x && isDigit y then Just (toLower x, read [y]) else Nothing
parseXY _ = Nothing

-- PTN: (stone)(square)
parsePlace :: String -> Maybe (StoneType, XY)
parsePlace (st:x:y:[]) = case readMaybe [toUpper st] of
  Just s -> case parseXY [x, y] of
    Just xy -> Just (s, xy)
    _ -> Nothing
  _ -> Nothing
parsePlace xy = case parseXY xy of
  Just xy -> Just (F, xy)
  _ -> Nothing

-- PTN: (count)(square)(direction)(drops count)(stone)
parseMove :: String -> Maybe Move
parseMove str = case parseMoveCount str of
  (count, x:y:d:drops) -> case parseXY (x:[y]) of
    Nothing -> Nothing
    Just xy -> case parseDir d of
      Nothing -> Nothing
      Just dir -> Just (count, xy, dir, (parseDrops count drops))
  -- not enough chars
  _ -> Nothing

-- default to 1
parseMoveCount :: String -> (Int, String)
parseMoveCount (h:str) = if isDigit h
  then (read [h], str)
  else (1, h:str)

parseDir :: Char -> Maybe Dir
parseDir c = case c of
  '<' -> Just West
  '>' -> Just East
  '+' -> Just North
  '-' -> Just South
  _ -> Nothing

parseDrops :: Int -> String -> Int
parseDrops c str = case reads str :: [(Int, String)] of
  [(drops, _)] -> drops
  [] -> c

-- actions

placeStone :: Board -> XY -> Stone -> Board
placeStone b xy s = map (stackStone xy s) b

stackStone :: XY -> Stone -> Cell -> Cell
stackStone (x,y) stone c@(Cell cx cy zs) = if x == cx && y == cy
  then Cell cx cy (zs ++ [stone])
  else c

placeStoneInGame :: Game -> XY -> StoneType -> (Game, Display)
placeStoneInGame g xy st = (g', str)
  where
    b = board g
    b' = placeStone b xy (Stone (getPlayer g) st)
    g' = g { board = b', turn = turn g + 1 }
    str = showBoardWithAxes b'

-- handlers

handleShow :: Game -> Either X Y -> (Game, Display)
handleShow g xory = case xory of
    Left x -> if isValidX g x then (g, showColByX b x) else (g, "Wrong x coordinate")
    Right y -> if isValidY g y then (g, showRowByY b y) else (g, "Wrong y coordinate")
  where
    b = board g

handlePlace :: Game -> XY -> StoneType -> (Game, Display)
handlePlace g xy st
  | not $ isValidXY g xy          = (g, "Wrong xy coordinates")
  | not $ canStack g xy           = (g, "The cell must be empty")
  | st == C && not (capsInDeck g) = (g, "No more caps in deck")
  | otherwise                     = placeStoneInGame g xy st

handleMove :: Game -> Move -> (Game, Display)
handleMove g m
  | not $ isValidMove g m     = (g, "Wrong args for move")
  | not $ isUnderControl g m  = (g, "You do not control the cell")
  | not $ isDropzoneClear g m = (g, "The dropzone is not clear")
  | otherwise                 = (g, show m)

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

-- IO

loop :: Game -> IO ()
loop g = do
  action <- prompt $ getPrompt g
  let (g', display) = handleAction g $ parseAction action
  putStrLn display
  case checkEnd g' of
    Just reason -> putStrLn reason
    _ -> loop g'

getPrompt :: Game -> Display
getPrompt g = "\nturn " ++ t ++ " / " ++ p ++ ">"
  where
    t = show $ turn g
    p = show $ getPlayer g

prompt :: String -> IO Display
prompt q = do
  putStr $ q ++ " "
  -- http://stackoverflow.com/questions/21853343/haskell-putstr-vs-putstrln-and-instruction-order
  hFlush stdout
  getLine

promptInt :: String -> IO (Maybe Int)
promptInt q = do
  a <- prompt q
  return (readMaybe a :: Maybe Int)

promptSize :: IO Int
promptSize = do
  size <- promptInt $ "Size of the board? [" ++ show minSize ++ ".." ++ show maxSize ++ "]"
  case size of
    Just s -> if isValidSize s then return s else promptSize
    Nothing -> promptSize

main = do
  putStrLn "Welcome to Tak.hs"
  size <- promptSize
  let g = Game { size = size, board = initBoard size, turn = 1 }
  putStrLn $ showGame g
  loop g
