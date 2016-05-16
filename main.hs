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

type Verb = String
type Coord = String
data Action = Action Verb [String]

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
isValidCoord :: Board -> Coord -> Bool
isValidCoord b c = True

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
showColByX :: Board -> Char -> String
showColByX b x = showCells $ cols !! (ord x - 97)
  where
    cols = toCols b

showRowByY :: Board -> Int -> String
showRowByY b y = showCells $ rows !! (y - 1)
  where
    rows = reverse $ toRows b

showBoardWithYAxis :: Board -> String
showBoardWithYAxis = unlines . reverse . map showRowWithY . toRows

showBoardWithAxes :: Board -> String
showBoardWithAxes b = showBoardWithYAxis b ++ "  " ++ showXAxis b

showRowWithY :: Row -> String
showRowWithY r = (show y) ++ " " ++ showCells r
  where
    (Cell _ y _) = head r

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

placeStone :: Board -> Coord -> Board
placeStone b coord = map (stackStone coord) b

stackStone :: Coord -> Cell -> Cell
stackStone (x:y) c@(Cell cx cy _) = if x == cx && (read y :: Int) == cy
  then Cell cx cy [Stone P1 F]
  else c

-- IO

handleShow :: Board -> String -> IO ()
handleShow b coord = do
  case isDigit $ head coord of
    True  -> putStrLn $ showRowByY b $ read coord
    False -> putStrLn $ showColByX b $ head coord

handlePlace :: Board -> Coord -> IO ()
handlePlace b coord = do
  case isValidCoord b coord of
    True -> putStrLn $ showBoardWithAxes $ placeStone b coord
    False -> putStrLn "Wrong coordinates xy"

handleAction :: Board -> Action -> IO ()
handleAction b a = do
  case a of
    (Action "show" (coord:cs)) -> handleShow b coord
    (Action "place" (coord:cs)) -> handlePlace b coord
    _ -> putStrLn "Unknown action"

loop :: Board -> Player -> IO ()
loop b p = do
  action <- prompt $ show p ++ ">"
  handleAction b $ parseAction action
  loop b p

prompt :: String -> IO String
prompt q = do
  putStr $ q ++ " "
  -- http://stackoverflow.com/questions/21853343/haskell-putstr-vs-putstrln-and-instruction-order
  hFlush stdout
  a <- getLine
  return a

main = do
  putStrLn "Welcome to Tak.hs"
  size <- prompt "Size of the board? [3..8]"
  let b = initBoard (read size :: Int)
  putStrLn $ showBoardWithAxes b
  loop b P1
