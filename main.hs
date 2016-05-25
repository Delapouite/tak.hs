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

import Data.Char (isAlpha, isDigit, ord, toLower, toUpper)
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

-- bottom to top
type Stack = [Stone]

data Cell = Cell Char Int Stack
instance Show Cell where
  show (Cell _ _ s) = showStack s

type Board = [Cell]
type Col = [Cell]
type Row = [Cell]
type X = Char
type Y = Int
type XY = (X, Y)

type Verb = String
data Action = Action Verb [String]

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

-- XY

getCell :: Board -> XY -> Maybe Cell
getCell b (x,y) = find (\(Cell cx cy _) -> cx == x && cy == y) b

getStacks :: Game -> [Stack]
getStacks g = map (\(Cell _ _ s) -> s) $ board g

-- 97 = ASCII 'a'
xToInt :: X -> Int
xToInt x = ord x - 97

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

canStack :: Game -> XY -> Bool
canStack g xy = isStackEmpty
  where
    cell = getCell (board g) xy
    isStackEmpty = case cell of
      Just (Cell _ _ stack) -> null stack
      Nothing -> False

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

getTallerStackHeight :: [Cell] -> Int
getTallerStackHeight cells = maximum $ map getStackHeight cells
  where
    getStackHeight (Cell _ _ stack) = length stack

-- display

showBoard :: Board -> String
showBoard = unlines . reverse . map showCells . toRows

showCells :: [Cell] -> String
showCells = unwords . map show

showStack :: Stack -> String
showStack [] = "."
showStack s = show $ last s

showStackLevel :: [Cell] -> Int -> String
showStackLevel cs i = unwords stones
  where
    stones = map getStone cs
    getStone (Cell _ _ stack) = if not (null stack) && (length stack - 1 >= i)
      then show $ stack !! i
      else " "

showStacks :: [Cell] -> String
showStacks cs = unlines $ map (showStackLevel cs) levels
  where
    levels = reverse [0..getTallerStackHeight cs - 1]

showColByX :: Board -> X -> String
showColByX b x = col ++ showYAxis b
  where
    cols = toCols b
    col = showStacks . reverse $ cols !! xToInt x

showRowByY :: Board -> Y -> String
showRowByY b y = row ++ showXAxis b
  where
    rows = toRows b
    row = showStacks $ rows !! (y - 1)

showBoardWithYAxis :: Board -> String
showBoardWithYAxis = unlines . reverse . map showRowWithY . toRows

showBoardWithAxes :: Board -> String
showBoardWithAxes b = "\n" ++ showBoardWithYAxis b ++ "  " ++ showXAxis b

showRowWithY :: Row -> String
showRowWithY r = show y ++ " " ++ showCells r
  where
    (Cell _ y _) = head r

-- horizontally
showYAxis :: Board -> String
showYAxis b = unwords $ map show $ reverse $ take (getSize b) [1..]

showXAxis :: Board -> String
showXAxis b = unwords $ map (: []) $ take (getSize b) xs

showDeck :: Game -> Player -> String
showDeck g p = show p ++ "'s deck: " ++ flats ++ " " ++ caps ++ "\n"
  where
    total = stoneCount $ size g
    totalCaps = capCount $ size g
    placedCaps = length $ getPlacedByPlayerAndType g p C
    placed = length (getPlacedByPlayer g p) - placedCaps
    flats = show (total - placed)
    caps = show (totalCaps - placedCaps) ++ show (Stone p C)

showDecks :: Game -> String
showDecks g = "\n" ++ showDeck g P1 ++ showDeck g P2

showGame :: Game -> String
showGame g = showDecks g ++ showBoardWithAxes (board g)

-- actions

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

placeStone :: Board -> XY -> Stone -> Board
placeStone b xy s = map (stackStone xy s) b

stackStone :: XY -> Stone -> Cell -> Cell
stackStone (x,y) stone c@(Cell cx cy stack) = if x == cx && y == cy
  then Cell cx cy (stack ++ [stone])
  else c

placeStoneInGame :: Game -> XY -> StoneType -> (Game, String)
placeStoneInGame g xy st = (g', str)
  where
    b = board g
    b' = placeStone b xy (Stone (getPlayer g) st)
    g' = g { board = b', turn = turn g + 1 }
    str = showBoardWithAxes b'

-- IO

handleShow :: Game -> Either X Y -> (Game, String)
handleShow g xory = case xory of
    Left x -> if isValidX g x then (g, showColByX b x) else (g, "Wrong x coordinate")
    Right y -> if isValidY g y then (g, showRowByY b y) else (g, "Wrong y coordinate")
  where
    b = board g

handlePlace :: Game -> XY -> StoneType -> (Game, String)
handlePlace g xy st
  | not $ isValidXY g xy = (g, "Wrong xy coordinates")
  | not $ canStack g xy  = (g, "The cell must be empty")
  | otherwise            = placeStoneInGame g xy st

handleAction :: Game -> Action -> (Game, String)
handleAction g a = case a of
    (Action "show" (coord:_)) -> handleShow g $ toXorY coord
    (Action "show" _) -> (g, showGame g)
    (Action "place" (args:_)) -> case parsePlace args of
      Just (st, xy) -> handlePlace g xy st
      _ -> (g, "Wrong stone type or xy coordinates")
    _ -> (g, "Unknown action")

loop :: Game -> IO ()
loop g = do
  action <- prompt $ getPrompt g
  let tup = handleAction g $ parseAction action
  putStrLn $ snd tup
  loop $ fst tup

getPrompt :: Game -> String
getPrompt g = "\nturn " ++ t ++ " / " ++ p ++ ">"
  where
    t = show $ turn g
    p = show $ getPlayer g

prompt :: String -> IO String
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
