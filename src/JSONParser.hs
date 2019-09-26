module JSONParser (parseJSON) where

import           JSONTypes
import           Control.Monad.Trans.State
import           Control.Monad.Fail as F (MonadFail, fail)
import           Control.Applicative
import           Control.Monad
import           Data.Char

type SParser m a = StateT String m a

ch :: MonadFail m => (Char -> Bool) -> SParser m Char
ch f = do
  (x:xs) <- get
  put xs
  if f x
    then return x
    else F.fail "Parse Error"

space :: (MonadPlus m, MonadFail m) => SParser m Char
space = ch (`elem` " \n\r\t")

token :: (MonadPlus m, MonadFail m) => SParser m a -> SParser m a
token = (*>) $ many space

tokenCh :: (MonadPlus m, MonadFail m) => Char -> SParser m Char
tokenCh x = token $ ch (== x)

matchStr :: MonadFail m => String -> SParser m Char
matchStr [] = ch $ const False
matchStr [x] = ch (== x)
matchStr (x:xs) = ch (== x) *> matchStr xs

matchConst :: MonadFail m => a -> String -> SParser m a
matchConst x y = x <$ matchStr y

parseStr :: (MonadPlus m, MonadFail m)
         => (SParser m Char -> SParser m String)
         -> SParser m String
parseStr x = token
  $ ch (== '\"') *> x (matchStr "\\\"" <|> ch (/= '\"')) <* ch (== '\"')

parseString :: (MonadPlus m, MonadFail m) => SParser m JSON
parseString = JString <$> parseStr many

parseNumber :: (MonadPlus m, MonadFail m) => SParser m JSON
parseNumber = JNumber . fromIntegral . read <$> some (ch isDigit)

parseTrue :: MonadFail m => SParser m JSON
parseTrue = JBool <$> matchConst True "true"

parseFalse :: MonadFail m => SParser m JSON
parseFalse = JBool <$> matchConst False "false"

parseBool :: (MonadPlus m, MonadFail m) => SParser m JSON
parseBool = parseTrue <|> parseFalse

parseNull :: MonadFail m => SParser m JSON
parseNull = matchConst JNull "null"

parseArr
  :: (MonadPlus m, MonadFail m) => Char -> Char -> SParser m a -> SParser m [a]
parseArr c c' x = (ch (== c) *> many (x <* tokenCh ',') <* tokenCh c')
  <|> (ch (== c) *> ((:) <$> x <*> many (tokenCh ',' *> x)) <* tokenCh c')

parseArray :: (MonadPlus m, MonadFail m) => SParser m JSON
parseArray = JArray <$> parseArr '[' ']' parseJSONData

parsePair :: (MonadPlus m, MonadFail m) => SParser m (String, JSON)
parsePair = (,) <$> parseStr some <* tokenCh ':' <*> parseJSONData

parseObject :: (MonadPlus m, MonadFail m) => SParser m JSON
parseObject = JObject <$> parseArr '{' '}' parsePair

parseJSONData :: (MonadPlus m, MonadFail m) => SParser m JSON
parseJSONData = token
  $ parseObject
  <|> parseArray
  <|> parseString
  <|> parseNumber
  <|> parseBool
  <|> parseNull

parseJSON :: (MonadPlus m, MonadFail m) => String -> m JSON
parseJSON = evalStateT parseJSONData