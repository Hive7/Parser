module Main where

import           JSONParser
import           EitherParser
import           System.IO

main :: IO ()
main = do
  handle <- openFile "./test/input.json" ReadMode
  contents <- hGetContents handle
  putStr contents
  print $ parseJSON' contents
  return ()
