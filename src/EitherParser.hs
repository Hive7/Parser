module EitherParser where -- Parser from scratch to gain a better understanding of the project

import           JSONTypes
import           Control.Applicative
import           Data.Bifunctor
import           Control.Monad
import           Data.Char

newtype JParser a = JParser (String -> Either JSONError (a, String))

parse' :: JParser a -> String -> Either JSONError (a, String)
parse' (JParser x) = x

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
      run (a, s') = parse' (g a) s'

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

leadingSpace' :: JParser a -> JParser a
leadingSpace' = (*>) $ many (parseChar' (`elem` " \t\r\n"))

spacedChar' :: Char -> JParser Char
spacedChar' x = leadingSpace' (parseChar' (== x))

matchString' :: String -> JParser String
matchString' = foldr (\x -> (<*>) $ (:) <$> parseChar' (== x)) $ pure []

parseString' :: (JParser Char -> JParser String) -> JParser String
parseString' x = parseChar' (== '\"')
  *> x (('\"' <$ matchString' "\\\"") <|> parseChar' (/= '\"'))
  <* parseChar' (== '\"')

parseNumber' :: JParser Double
parseNumber' = read <$> some (parseChar' isDigit <|> parseChar' (`elem` "-e."))

parseBool' :: JParser Bool
parseBool' = (== "true") <$> (matchString' "true" <|> matchString' "false")

parseNull' :: JParser JSON
parseNull' = JNull <$ matchString' "null"

parseSepBy' :: JParser a -> JParser b -> JParser [a]
parseSepBy' p sep = (((:) <$> p <*> many (sep *> p)) <|> pure []) <* many sep

parseArr' :: Char -> Char -> JParser a -> JParser [a]
parseArr' c c' x = parseChar' (== c) *> parseSepBy' x (spacedChar' ',')
  <* spacedChar' c'

parseArray' :: JParser [JSON]
parseArray' = parseArr' '[' ']' parseJSONData'

parseObject' :: JParser [(String, JSON)]
parseObject' = parseArr' '{' '}'
  $ leadingSpace'
  $ (,) <$> (parseString' some <* spacedChar' ':') <*> parseJSONData'

parseJSONData' :: JParser JSON
parseJSONData' = leadingSpace'
  $ (JObject <$> parseObject')
  <|> (JArray <$> parseArray')
  <|> parseNull'
  <|> (JBool <$> parseBool')
  <|> (JNumber <$> parseNumber')
  <|> (JString <$> parseString' many)

parseJSON' :: String -> Either JSONError JSON
parseJSON' x = fst <$> parse' parseJSONData' x