module Display where

import Data.Char (toLower)
import Text.PrettyPrint.ANSI.Leijen

import Tak
import Board
import Conversion
import Option

toColor1 :: String -> String
toColor1 = show . blue . text

toColor2 :: String -> String
toColor2 = show . yellow . text

showPlayer :: Colored -> Player -> Display
showPlayer True P1 = toColor1 "P1"
showPlayer True P2 = toColor2 "P2"
showPlayer _ p = show p

showCell :: Colored -> Cell -> Display
showCell colored (Cell _ zs) = showStack colored zs

showStone :: Colored -> Stone -> Display
showStone True (Stone P1 t) = toColor1 $ show t
showStone True (Stone P2 t) = toColor2 $ map toLower $ show t
showStone _ (Stone P1 t) = show t
showStone _ (Stone P2 t) = map toLower $ show t

showBoard :: Colored -> Board -> Display
showBoard colored = unlines . reverse . map (showCells colored) . toRows

showCells :: Colored -> [Cell] -> Display
showCells colored = unwords . map (showCell colored)

showStack :: Colored -> Stack -> Display
showStack _ [] = "."
showStack colored zs = showStone colored $ last zs

showStackLevel :: Colored -> [Cell] -> Int -> Display
showStackLevel colored cs i = unwords stones
  where
    stones = map getStone cs
    getStone (Cell _ zs) = if not (null zs) && (length zs - 1 >= i)
      then showStone colored $ zs !! i
      else " "

showStacks :: Colored -> [Cell] -> Display
showStacks colored cs = unlines $ map (showStackLevel colored cs) levels
  where
    levels = reverse [0..getMaxHeight cs - 1]

showCol :: Colored -> Board -> X -> Display
showCol colored b x = col ++ showYAxis b
  where
    col = showStacks colored . reverse $ getCol b x

showRow :: Colored -> Board -> Y -> Display
showRow colored b y = row ++ showXAxis b
  where
    row = showStacks colored $ getRow b y

showBoardWithYAxis :: Colored -> Board -> Display
showBoardWithYAxis colored = unlines . reverse . map (showRowWithY colored) . toRows

showBoardWithAxes :: Colored -> Board -> Display
showBoardWithAxes colored b = "\n" ++ showBoardWithYAxis colored b ++ "  " ++ showXAxis b

showRowWithY :: Colored -> Row -> Display
showRowWithY colored r = show y ++ " " ++ showCells colored r
  where
    (Cell (_, y) _) = head r

-- horizontally
showYAxis :: Board -> Display
showYAxis b = unwords $ map show $ reverse $ take (getSize b) [1..]

showXAxis :: Board -> Display
showXAxis b = unwords $ map (: []) $ take (getSize b) xs

showDeck :: Game -> Player -> Display
showDeck g p = showPlayer (inColors g) p ++ "'s deck: " ++ flats ++ " " ++ caps ++ "\n"
  where
    b = board g
    total = stoneCount $ size g
    totalCaps = capCount $ size g
    placedCaps = length $ getPlacedByPlayerAndType b p C
    placed = length (getPlacedByPlayer b p) - placedCaps
    flats = show (total - placed)
    caps = show (totalCaps - placedCaps) ++ showStone (inColors g) (Stone p C)

showDecks :: Game -> Display
showDecks g = "\n" ++ showDeck g P1 ++ showDeck g P2

showGame :: Game -> Display
showGame g = showDecks g ++ showBoardWithAxes (inColors g) (board g)
