module StateParser where

import           JSONTypes
import           Control.Monad.Trans.State
import           Control.Monad.Fail as F (MonadFail, fail)
import           Control.Applicative
import           Control.Monad
import           Data.Char

type SParser m a = StateT String m a

parse :: Monad m => String -> SParser m a -> m (a, String)
parse = flip runStateT

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
matchstr [x] = ch $ const True
matchstr (x:xs) = ch (== x) *> matchstr xs

matchconst :: MonadFail m => a -> String -> SParser m a
matchconst x y = x <$ matchstr y

parseStr :: (MonadPlus m, MonadFail m) => SParser m String
parseStr = token $ ch (== '\"') *> many (ch (/= '\"')) <* ch (== '\"')

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

parseArray :: (MonadPlus m, MonadFail m) => SParser m JSON
parseArray = JArray
  <$> (ch (== '[') *> many (parseJSON <* tokench ',') <* tokench ']')

parseObject :: (MonadPlus m, MonadFail m) => SParser m JSON
parseObject = JObject
  <$> (ch (== '{')
       *> many ((,) <$> parseStr <* tokench ':' <*> (parseJSON <* tokench ','))
       <* tokench '}')

parseJSON :: (MonadPlus m, MonadFail m) => SParser m JSON
parseJSON = token
  $ parseObject
  <|> parseArray
  <|> parseString
  <|> parseNumber
  <|> parseBool
  <|> parseNull

parseMany :: (MonadPlus m, MonadFail m) => SParser m [JSON]
parseMany = many parseJSON