-- https://www.youtube.com/watch?v=iEXkpS-Q9dI

-- Coordinates system according to Portable Tak Notation
-- 3
-- 2
-- 1
--  a b c

import Data.Char (isDigit, ord)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import System.IO (hFlush, stdout)

-- types

data Player = P1 | P2 deriving (Show)

-- Flat, Standing or Cap
data StoneType = F | S | C deriving (Show, Read)

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
type XY = (Char, Int)

type Verb = String
data Action = Action Verb [String]

data Game = Game { size :: Int
                 , board :: Board
                 , turn :: Int
                 }

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
getPlayer g = if (turn g) `mod` 2 == 0 then P2 else P1

-- XY

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

argToXY :: String -> XY
argToXY (x:y) = (x, read y :: Int)

argToXorY :: String -> Either X Y
argToXorY arg = case isDigit $ head arg of
  False -> Left  (head arg)
  True  -> Right (read arg :: Int)

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
    getStone (Cell _ _ stack) = if (not $ null stack) && (length stack - 1 >= i)
      then show $ stack !! i
      else " "

showStacks :: [Cell] -> String
showStacks cs = unlines $ map (showStackLevel cs) levels
  where
    levels = reverse [0..(getTallerStackHeight cs) - 1]

showColByX :: Board -> X -> String
showColByX b x = col ++ showYAxis b
  where
    cols = toCols b
    col = showStacks . reverse $ cols !! (xToInt x)

showRowByY :: Board -> Y -> String
showRowByY b y = row ++ showXAxis b
  where
    rows = toRows b
    row = showStacks $ rows !! (y - 1)

showBoardWithYAxis :: Board -> String
showBoardWithYAxis = unlines . reverse . map showRowWithY . toRows

showBoardWithAxes :: Board -> String
showBoardWithAxes b = showBoardWithYAxis b ++ "  " ++ showXAxis b

showRowWithY :: Row -> String
showRowWithY r = (show y) ++ " " ++ showCells r
  where
    (Cell _ y _) = head r

-- horizontally
showYAxis :: Board -> String
showYAxis b = unwords $ map show $ reverse $ take (getSize b) [1..]

showXAxis :: Board -> String
showXAxis b = unwords $ map (\x -> x : []) $ take (getSize b) xs

-- actions

parseAction :: String -> Action
parseAction s = Action verb args
  where
    (verb:args) = words s

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
    g' = g { board = b', turn = (turn g) + 1 }
    str = showBoardWithAxes b'

-- IO

handleShow :: Game -> Either X Y -> (Game, String)
handleShow g xory = do
  let b = board g
  case xory of
    Left x -> if isValidX g x then (g, showColByX b x) else (g, "Wrong coordinate x")
    Right y -> if isValidY g y then (g, showRowByY b y) else (g, "Wrong coordinate y")

handlePlace :: Game -> XY -> StoneType-> (Game, String)
handlePlace g xy st = do
  case isValidXY g xy of
    True -> placeStoneInGame g xy st
    False -> (g, "Wrong coordinates xy")

handleAction :: Game -> Action -> (Game, String)
handleAction g a = do
  case a of
    (Action "show" (coord:_)) -> handleShow g $ argToXorY coord
    (Action "place" (coord:s:_)) -> handlePlace g (argToXY coord) (read s :: StoneType)
    (Action "place" (coord:_)) -> handlePlace g (argToXY coord) F
    _ -> (g, "Unknown action")

loop :: Game -> IO ()
loop g = do
  action <- prompt $ getPrompt g
  let tup = handleAction g $ parseAction action
  putStrLn $ snd tup
  loop $ fst tup

getPrompt :: Game -> String
getPrompt g = "turn " ++ t ++ " / " ++ p ++ ">"
  where
    t = show $ turn g
    p = show $ getPlayer g

prompt :: String -> IO String
prompt q = do
  putStr $ q ++ " "
  -- http://stackoverflow.com/questions/21853343/haskell-putstr-vs-putstrln-and-instruction-order
  hFlush stdout
  a <- getLine
  return a

promptInt q = do
  a <- prompt q
  return (read a :: Int)

main = do
  putStrLn "Welcome to Tak.hs"
  size <- promptInt "Size of the board? [3..8]"
  let b = initBoard size
  let g = Game { size=size, board=b, turn=1 }
  putStrLn $ showBoardWithAxes b
  loop g
