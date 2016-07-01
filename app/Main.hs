module Main where

-- https://www.youtube.com/watch?v=iEXkpS-Q9dI

import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

import Tak
import Action
import Board
import Cell
import Conversion
import Display
import Parser
import Validation

-- IO / prompts

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
    p = show $ player g

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

promptSize :: IO Size
promptSize = do
  size <- promptInt $ "Size of the board? [" ++ show minSize ++ ".." ++ show maxSize ++ "]"
  case size of
    Just s -> if isValidSize s then return s else promptSize
    Nothing -> promptSize

main = do
  putStrLn "Welcome to Tak.hs"
  size <- promptSize
  let g = Game { size = size, board = initBoard size, player = P1, turn = 1 }
  putStrLn $ showGame g
  loop g
