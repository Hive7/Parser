-- Parser from scratch to gain a better understanding of the project
module EitherParser where

import           JSONTypes
import           Control.Applicative
import           Data.Bifunctor
import           Control.Monad
import           Data.Char

newtype JParser a = JParser (String -> Either JSONError (a, String))

instance Functor JParser where
  fmap f (JParser g) = JParser $ fmap y . g
    where
      y (a, b) = (f a, b)

instance Applicative JParser where
  pure x = JParser $ \s -> pure (x, s)

  JParser f <*> JParser g = JParser $ f >=> run
    where
      run (f', s') = first f' <$> g s'

instance Monad JParser where
  return = pure

  JParser f >>= g = JParser $ f >=> run
    where
      run (a, s') = let JParser h = g a
                    in h s'

instance Alternative JParser where
  empty = JParser $ const $ Left JUnknown

  JParser f <|> JParser g = JParser $ \s -> either (const $ g s) Right $ f s

parseChar' :: (Char -> Bool) -> JParser Char
parseChar' f = JParser
  $ \s -> case s of
    []     -> Left JEmpty
    (x:xs)
      | f x -> pure (x, xs)
    (x:_)  -> Left $ JUnexpectedCharacter x

matchString' :: String -> JParser String
matchString' = foldr (\x -> (<*>) $ (:) <$> parseChar' (== x)) $ pure []

parseNumber' :: (Read i, Integral i) => JParser i
parseNumber' = read <$> some (parseChar' isDigit)

parseBool' :: JParser Bool
parseBool' = (== "true") <$> (matchString' "true" <|> matchString' "false")

parseNull' :: JParser JSON
parseNull' = JNull <$ matchString' "null"

parseSepBy' :: JParser a -> JParser b -> JParser [a]
parseSepBy' p sep = (((:) <$> p <*> many (sep *> p)) <|> pure []) <* many sep

parse' :: JParser a -> String -> Either JSONError (a, String)
parse' (JParser x) = x

