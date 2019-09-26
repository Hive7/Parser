module Lib where

import           JSONTypes
import           Data.Functor
import           Control.Applicative
import           Control.Monad.Trans.State

data Parser a = MkParser (String -> Maybe (a, String))

instance Functor Parser where
  fmap f (MkParser y) = MkParser
    $ \x -> case y x of
      Just (a, b) -> Just (f a, b)
      Nothing     -> Nothing

instance Applicative Parser where
  pure a = MkParser $ \x -> Just (a, x)

  MkParser a <*> MkParser b = MkParser
    $ \x -> case a x of
      Nothing     -> Nothing
      Just (y, z) -> case b z of
        Nothing     -> Nothing
        Just (c, d) -> Just (y c, d)

instance Alternative Parser where
  empty = MkParser $ const Nothing

  MkParser a <|> MkParser b = MkParser
    $ \x -> case a x of
      Nothing -> b x
      c       -> c