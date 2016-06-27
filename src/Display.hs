module Display where

import Tak
import Conversion

instance Show Stone where
  show (Stone P1 F) = "F"
  show (Stone P2 F) = "f"
  show (Stone P1 S) = "S"
  show (Stone P2 S) = "s"
  show (Stone P1 C) = "C"
  show (Stone P2 C) = "c"

instance Show Cell where
  show (Cell _ zs) = showStack zs

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
    getStone (Cell _ zs) = if not (null zs) && (length zs - 1 >= i)
      then show $ zs !! i
      else " "

showStacks :: [Cell] -> Display
showStacks cs = unlines $ map (showStackLevel cs) levels
  where
    levels = reverse [0..getMaxHeight cs - 1]

showCol :: Board -> X -> Display
showCol b x = col ++ showYAxis b
  where
    col = showStacks . reverse $ getCol b x

showRow :: Board -> Y -> Display
showRow b y = row ++ showXAxis b
  where
    row = showStacks $ getRow b y

showBoardWithYAxis :: Board -> Display
showBoardWithYAxis = unlines . reverse . map showRowWithY . toRows

showBoardWithAxes :: Board -> Display
showBoardWithAxes b = "\n" ++ showBoardWithYAxis b ++ "  " ++ showXAxis b

showRowWithY :: Row -> Display
showRowWithY r = show y ++ " " ++ showCells r
  where
    (Cell (_, y) _) = head r

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


