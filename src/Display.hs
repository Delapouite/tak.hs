module Display where

import Control.Monad.Reader
import Data.Char (toLower)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Tak
import Board
import Conversion

type ROptions = Reader Options

emptyCell = "."

toColor :: Player -> String -> String
toColor P1 = show . blue . text
toColor P2 = show . yellow . text

-- order from small to big components

-- in prompt or decks
showPlayer :: Player -> ROptions Display
showPlayer p = do
  colored <- asks optColors
  return $ if colored then toColor p $ show p else show p

-- capital letters for P1
showStoneType :: StoneType -> Player -> Display
showStoneType t P1 = show t
showStoneType t P2 = map toLower $ show t

-- stoneType
showStone :: Stone -> ROptions Display
showStone (Stone p t) = do
  colored <- asks optColors
  let disp = showStoneType t p
  return $ if colored then toColor p disp else disp

showStoneAtLevel :: Int -> Cell -> ROptions Display
showStoneAtLevel lvl (Cell _ zs)
  | not (null zs) && (length zs - 1 >= lvl) = showStone (reverse zs !! lvl)
  | lvl == 0  = return emptyCell
  | otherwise = return " "

-- top view
showStack :: Stack -> ROptions Display
showStack [] = return emptyCell
showStack (s:ss) = showStone s

showStackLevel :: [Cell] -> Int -> ROptions Display
showStackLevel cs lvl = do
  stones <- mapM (showStoneAtLevel lvl) cs
  return $ unwords stones

-- side view
showStacks :: [Cell] -> ROptions Display
showStacks cs = do
  let levels = reverse [0..getMaxHeight cs - 1]
  stackLevels <- mapM (showStackLevel cs) levels
  return $ if null stackLevels
      then unwords (map (const emptyCell) cs) ++ "\n"
      else unlines stackLevels

-- top view
showCell :: Cell -> ROptions Display
showCell (Cell _ zs) = showStack zs

-- col or row
showCells :: [Cell] -> ROptions Display
showCells cs = do
  cells <- mapM showCell cs
  return $ unwords cells

-- side view: stacks + axis
showCol :: Board -> X -> ROptions Display
showCol b x = do
  col <- showStacks . reverse $ getCol b x
  return $ "\n" ++ col ++ showYAxis b

-- side view: stacks + axis
showRow :: Board -> Y -> ROptions Display
showRow b y = do
  row <- showStacks $ getRow b y
  return $ "\n" ++ row ++ showXAxis b

showRowWithYSmall :: Y -> Row -> ROptions Display
showRowWithYSmall y r = do
  cells <- showCells r
  return $ show y ++ " " ++ cells

showRowWithYBig :: Y -> Row -> ROptions Display
showRowWithYBig y r = do
  stacks <- lines <$> showStacks r
  let i = map (\s -> "  " ++ s) $ init stacks
  let l = show y ++ " " ++ last stacks
  return $ unlines $ i ++ [l]

-- top view
showRowWithY :: Row -> ROptions Display
showRowWithY r = do
  big <- asks optBig
  let (Cell (_, y) _) = head r
  if big
    then showRowWithYBig y r
    else showRowWithYSmall y r

showBoard :: Board -> ROptions Display
showBoard b = do
  rows <- mapM showCells $ toRows b
  return $ unlines . reverse $ rows

showBoardWithYAxis :: Board -> ROptions Display
showBoardWithYAxis b = do
  rows <- mapM showRowWithY $ toRows b
  return $ unlines . reverse $ rows

showBoardWithAxes :: Board -> ROptions Display
showBoardWithAxes b = do
  withYAxis <- showBoardWithYAxis b
  return $ "\n" ++ withYAxis ++ "  " ++ showXAxis b

-- horizontally 5 4 3 2 1
showYAxis :: Board -> Display
showYAxis b = unwords . map show . reverse $ take (getSize b) [1..]

-- a b c d e
showXAxis :: Board -> Display
showXAxis b = unwords . map (: []) $ take (getSize b) xs

showDeck :: Game -> Player -> Display
showDeck g p = let
  b = board g
  total = stoneCount $ size g
  totalCaps = capCount $ size g
  placedCaps = length $ getPlacedByPlayerAndType b p C
  placed = length (getPlacedByPlayer b p) - placedCaps
  flats = show (total - placed)
  cap = runReader (showStone (Stone p C)) (options g)
  caps = show (totalCaps - placedCaps) ++ cap
  p' = runReader (showPlayer p) (options g)
  in p' ++ "'s deck: " ++ flats ++ " " ++ caps ++ "\n"

showDecks :: Game -> Display
showDecks g = "\n" ++ showDeck g P1 ++ showDeck g P2

showGame :: Game -> Display
showGame g = showDecks g ++ runReader (showBoardWithAxes (board g)) (options g)
