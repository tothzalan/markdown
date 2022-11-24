module Main (main) where

import System.Environment (getArgs)
import Lib (interpret)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> putStr ""
    _ -> error "Wrong number of arguments!"
  fileString <- do readFile (head args)
  let file = lines fileString
  interpret file
