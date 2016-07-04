module Display where

import Control.Monad.Reader
import Data.Char (toLower)
import Text.PrettyPrint.ANSI.Leijen

import Tak
import Board
import Conversion

type ROptions = Reader Options

toColor :: Player -> String -> String
toColor P1 = show . blue . text
toColor P2 = show . yellow . text

showPlayer :: Player -> ROptions Display
showPlayer p = do
  colored <- asks optColors
  return $ if colored then toColor p $ show p else show p

showStone :: Stone -> ROptions Display
showStone (Stone p t) = do
  colored <- asks optColors
  let disp = showStoneType t p
  return $ if colored then toColor p disp else disp

showStoneType :: StoneType -> Player -> Display
showStoneType t P1 = show t
showStoneType t P2 = map toLower $ show t

showBoard :: Board -> ROptions Display
showBoard b = do
  rows <- mapM showCells $ toRows b
  return $ unlines . reverse $ rows

showCell :: Cell -> ROptions Display
showCell (Cell _ zs) = showStack zs

showCells :: [Cell] -> ROptions Display
showCells cs = do
  cells <- mapM showCell cs
  return $ unwords cells

showStack :: Stack -> ROptions Display
showStack [] = return "."
showStack zs = showStone $ last zs

showStoneAtLevel :: Int -> Cell -> ROptions Display
showStoneAtLevel lvl (Cell _ zs) =
  if not (null zs) && (length zs - 1 >= lvl)
  then showStone (zs !! lvl)
  else return " "

showStackLevel :: [Cell] -> Int -> ROptions Display
showStackLevel cs lvl = do
  stones <- mapM (showStoneAtLevel lvl) cs
  return $ unwords stones

showStacks :: [Cell] -> ROptions Display
showStacks cs = do
  let levels = reverse [0..getMaxHeight cs - 1]
  stackLevels <- mapM (showStackLevel cs) levels
  return $ unlines stackLevels

showCol :: Board -> X -> ROptions Display
showCol b x = do
  col <- showStacks . reverse $ getCol b x
  return $ col ++ showYAxis b

showRow :: Board -> Y -> ROptions Display
showRow b y = do
  row <- showStacks $ getRow b y
  return $ row ++ showXAxis b

showBoardWithYAxis :: Board -> ROptions Display
showBoardWithYAxis b = do
  rows <- mapM showRowWithY $ toRows b
  return $ unlines . reverse $ rows

showBoardWithAxes :: Board -> ROptions Display
showBoardWithAxes b = do
  withYAxis <- showBoardWithYAxis b
  return $ "\n" ++ withYAxis ++ "  " ++ showXAxis b

showRowWithY :: Row -> ROptions Display
showRowWithY r = do
  let (Cell (_, y) _) = head r
  cells <- showCells r
  return $ show y ++ " " ++ cells

-- horizontally
showYAxis :: Board -> Display
showYAxis b = unwords $ map show $ reverse $ take (getSize b) [1..]

showXAxis :: Board -> Display
showXAxis b = unwords $ map (: []) $ take (getSize b) xs

showDeck :: Game -> Player -> Display
showDeck g p = p' ++ "'s deck: " ++ flats ++ " " ++ caps ++ "\n"
  where
    b = board g
    total = stoneCount $ size g
    totalCaps = capCount $ size g
    placedCaps = length $ getPlacedByPlayerAndType b p C
    placed = length (getPlacedByPlayer b p) - placedCaps
    flats = show (total - placed)
    cap = runReader (showStone (Stone p C)) (options g)
    caps = show (totalCaps - placedCaps) ++ cap
    p' = runReader (showPlayer p) (options g)

showDecks :: Game -> Display
showDecks g = "\n" ++ showDeck g P1 ++ showDeck g P2

showGame :: Game -> Display
showGame g = showDecks g ++ runReader (showBoardWithAxes (board g)) (options g)
