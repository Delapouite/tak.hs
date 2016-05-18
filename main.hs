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
data StoneType = F | S | C deriving (Show)

-- P1 in UpperCase, P2 in LowerCase
data Stone = Stone Player StoneType
instance Show Stone where
  show (Stone P1 F) = "F"
  show (Stone P2 F) = "f"

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

-- TODO
getPlayer :: Game -> Player
getPlayer g = if (turn g) `mod` 2 == 0 then P2 else P1

-- TODO
isValidXY :: Board -> XY -> Bool
isValidXY b xy = True

toCols :: Board -> [Col]
toCols b = chunksOf (getSize b) b

toRows :: Board -> [Row]
toRows = transpose . toCols

-- display

showBoard :: Board -> String
showBoard = unlines . reverse . map showCells . toRows

showCells :: [Cell] -> String
showCells = unwords . map show

-- 97 = ASCII 'a'
showColByX :: Board -> X -> String
showColByX b x = col ++ "\n" ++ showYAxis b
  where
    cols = toCols b
    col = showCells . reverse $ cols !! (ord x - 97)

showRowByY :: Board -> Y -> String
showRowByY b y = row ++ "\n" ++ showXAxis b
  where
    rows = toRows b
    row = showCells $ rows !! (y - 1)

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

showStack :: Stack -> String
showStack [] = "."
showStack s = show $ last s

-- actions

parseAction :: String -> Action
parseAction s = Action verb args
  where
    (verb:args) = words s

argToXY :: String -> XY
argToXY arg = (x, y)
  where
    (f:s) = arg
    x = f
    y = read s :: Int

argToXorY :: String -> Either X Y
argToXorY arg = case isDigit $ head arg of
  False -> Left  (head arg)
  True  -> Right (read arg :: Int)

placeStone :: Board -> XY -> Board
placeStone b xy = map (stackStone xy) b

stackStone :: XY -> Cell -> Cell
stackStone (x,y) c@(Cell cx cy _) = if x == cx && y == cy
  then Cell cx cy [Stone P1 F]
  else c

-- TODO
placeStoneInGame :: Game -> XY -> (Game, String)
placeStoneInGame g xy = (g', str)
  where
    b = board g
    b' = placeStone b xy
    g' = g { board = b', turn = (turn g) + 1 }
    str = showBoardWithAxes b'

-- IO

handleShow :: Game -> Either X Y -> (Game, String)
handleShow g xory = do
  let b = board g
  case xory of
    Left x -> (g, showColByX b x)
    Right y -> (g, showRowByY b y)

handlePlace :: Game -> XY -> (Game, String)
handlePlace g xy = do
  let b = board g
  case isValidXY b xy of
    True -> placeStoneInGame g xy 
    False -> (g, "Wrong coordinates xy")

handleAction :: Game -> Action -> (Game, String)
handleAction g a = do
  case a of
    (Action "show" (coord:_)) -> handleShow g $ argToXorY coord
    (Action "place" (coord:_)) -> handlePlace g $ argToXY coord
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
