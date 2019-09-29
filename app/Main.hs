module Main where

import           JSONParser
import           EitherParser
import           System.IO
import           Network.Wreq
import           Data.ByteString.Lazy (unpack, ByteString)
import           Control.Lens
import           Data.Char

-- Data Sources:
-- - https://data.nasa.gov/resource/y77d-th95.json - NASA Earth Meteorite Landings
main :: IO ()
main = do
  -- handle <- openFile "./test/input-1.json" ReadMode
  -- contents <- hGetContents handle
  contents
   <- respToString <$> get "https://data.nasa.gov/resource/y77d-th95.json"
  print $ parseJSON' contents
  -- hClose handle
  return ()

respToString :: Response ByteString -> String
respToString = map (chr . fromEnum) . unpack . (^. responseBody)