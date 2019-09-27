{-# LANGUAGE ConstraintKinds #-}

module JSONParser (parseJSON) where

import           JSONTypes
import           Control.Monad.Trans.State
import           Control.Monad.Fail as F (MonadFail, fail)
import           Control.Applicative
import           Control.Monad
import           Data.Char

type SParser = StateT String

type FCst m = MonadFail m

type FPCst m = (MonadPlus m, FCst m)

ch :: FCst m => (Char -> Bool) -> SParser m Char
ch f = do
  (x:xs) <- get
  put xs
  if f x
    then return x
    else F.fail "Parse Error"

space :: FCst m => SParser m Char
space = ch (`elem` " \n\r\t")

token :: FPCst m => SParser m a -> SParser m a
token = (*>) $ many space

tokenCh :: FPCst m => Char -> SParser m Char
tokenCh x = token $ ch (== x)

matchStr :: FCst m => String -> SParser m Char
matchStr [] = ch $ const False
matchStr [x] = ch (== x)
matchStr (x:xs) = ch (== x) *> matchStr xs

matchConst :: FCst m => a -> String -> SParser m a
matchConst x y = x <$ matchStr y

parseStr :: FPCst m => (SParser m Char -> SParser m String) -> SParser m String
parseStr x = token
  $ ch (== '\"') *> x (matchStr "\\\"" <|> ch (/= '\"')) <* ch (== '\"')

parseString :: FPCst m => SParser m JSON
parseString = JString <$> parseStr many

parseNumber :: FPCst m => SParser m JSON
parseNumber = JNumber . fromIntegral . read <$> some (ch isDigit)

parseTrue :: FCst m => SParser m JSON
parseTrue = JBool <$> matchConst True "true"

parseFalse :: FCst m => SParser m JSON
parseFalse = JBool <$> matchConst False "false"

parseBool :: FPCst m => SParser m JSON
parseBool = parseTrue <|> parseFalse

parseNull :: FCst m => SParser m JSON
parseNull = matchConst JNull "null"

parseArr :: FPCst m => Char -> Char -> SParser m a -> SParser m [a]
parseArr c c' x = ch (== c) *> sepBy x (tokenCh ',') <* tokenCh c'

sepBy :: FPCst m => SParser m a -> SParser m b -> SParser m [a]
sepBy p sep = (((:) <$> p <*> many (sep *> p)) <|> pure []) <* many sep

parseArray :: FPCst m => SParser m JSON
parseArray = JArray <$> parseArr '[' ']' parseJSONData

parsePair :: FPCst m => SParser m (String, JSON)
parsePair = (,) <$> parseStr some <* tokenCh ':' <*> parseJSONData

parseObject :: FPCst m => SParser m JSON
parseObject = JObject <$> parseArr '{' '}' parsePair

parseJSONData :: FPCst m => SParser m JSON
parseJSONData = token
  $ parseObject
  <|> parseArray
  <|> parseString
  <|> parseNumber
  <|> parseBool
  <|> parseNull

parseJSON :: FPCst m => String -> m JSON
parseJSON = evalStateT parseJSONData