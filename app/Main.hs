module Main where

import           JSONParser
import           System.IO

main :: IO ()
main = do
  handle <- openFile "./test/input.json" ReadMode
  contents <- hGetContents handle
  putStr contents
  json <- parseJSON contents
  print json
  return ()
