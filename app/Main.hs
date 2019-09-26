module Main where

import           Lib
import           StateParser
import           System.IO

main :: IO ()
main = do
  handle <- openFile "./test/input-1.json" ReadMode
  contents <- hGetContents handle
  putStr contents
  json <- fst <$> parse contents parseJSON
  print json
  return ()
