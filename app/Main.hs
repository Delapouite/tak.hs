module Main where

-- https://www.youtube.com/watch?v=iEXkpS-Q9dI

import Control.Monad.Reader
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Tak
import Board
import Cell
import Command
import Conversion
import Display
import Parser
import Validation

-- options

optDescrs :: [OptDescr (Options -> IO Options)]
optDescrs =
  [ Option "c" ["colors"]
      (NoArg
        (\opt -> return opt { optColors = True }))
      "enable colors"
  ]

-- IO / prompts

loop :: Game -> IO ()
loop g = do
  command <- prompt $ getPrompt g
  let (g', display) = handleCommand g $ parseCommand command
  putStrLn display
  case checkEnd g' of
    Just reason -> putStrLn reason
    _ -> loop g'

getPrompt :: Game -> Display
getPrompt g = "\nturn " ++ t ++ " / " ++ p ++ ">"
  where
    t = show $ turn g
    p = runReader (showPlayer (player g)) (options g)

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

  args <- getArgs
  let (flags, _, _) = getOpt RequireOrder optDescrs args
  opts <- foldl (>>=) (return defaultOptions) flags

  size <- promptSize
  let g = Game { size = size
               , board = initBoard size
               , player = P1
               , turn = 1
               , options = opts
               }

  putStrLn $ showGame g
  loop g
