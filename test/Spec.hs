{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.QuickCheck
import           Control.Monad.Trans.State
import           JSONParser
import           JSONTypes
import           Data.Either

main :: IO ()
main = hspec
  $ do
    describe "JSONParse.ch"
      $ do
        prop "Matches a character at the start of an input"
          $ \(x :: Char) (y :: String) -> either (const False) (== x)
          $ evalStateT (ch (== x)) (x:y)
        prop "Fails an empty string"
          $ \(x :: Char) -> isLeft $ evalStateT (ch (== x)) ""
    describe "JSONParse.space" $ return ()
