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
space = ch (`elem` " \n\r")

token :: (MonadPlus m, MonadFail m) => SParser m a -> SParser m a
token = (*>) $ many space

tokench :: (MonadPlus m, MonadFail m) => Char -> SParser m Char
tokench x = token $ ch (== x)

matchstr :: MonadFail m => String -> SParser m Char
matchstr [] = ch $ const False
matchstr [x] = ch (== x)
matchstr (x:xs) = ch (== x) *> matchstr xs

matchconst :: MonadFail m => a -> String -> SParser m a
matchconst x y = x <$ matchstr y

parseStr :: (MonadPlus m, MonadFail m) => SParser m String
parseStr = token
  $ ch (== '\"') *> many (matchstr "\\\"" <|> ch (/= '\"')) <* ch (== '\"')

parseString :: (MonadPlus m, MonadFail m) => SParser m JSON
parseString = JString <$> parseStr

parseNumber :: (MonadPlus m, MonadFail m) => SParser m JSON
parseNumber = JNumber . fromIntegral . read <$> some (ch isDigit)

parseTrue :: MonadFail m => SParser m JSON
parseTrue = JBool <$> matchconst True "true"

parseFalse :: MonadFail m => SParser m JSON
parseFalse = JBool <$> matchconst False "false"

parseBool :: (MonadPlus m, MonadFail m) => SParser m JSON
parseBool = parseTrue <|> parseFalse

parseNull :: MonadFail m => SParser m JSON
parseNull = matchconst JNull "null"

parseArr
  :: (MonadPlus m, MonadFail m) => Char -> Char -> SParser m a -> SParser m [a]
parseArr c c' x = (ch (== c) *> many (x <* tokench ',') <* tokench c')
  <|> (ch (== c) *> ((:) <$> x <*> many (tokench ',' *> x)) <* tokench c')

parseArray :: (MonadPlus m, MonadFail m) => SParser m JSON
parseArray = JArray <$> (parseArr '[' ']' parseJSONData)

parsePair :: (MonadPlus m, MonadFail m) => SParser m (String, JSON)
parsePair = (,) <$> parseStr <* tokench ':' <*> parseJSONData

parseObject :: (MonadPlus m, MonadFail m) => SParser m JSON
parseObject = JObject <$> (parseArr '{' '}' parsePair)

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