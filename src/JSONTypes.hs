module JSONTypes where

import           Control.Monad
import           Control.Applicative

data JSON = JString String
          | JNumber Double
          | JBool Bool
          | JNull
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving (Show, Eq, Ord)

data JSONError = JEmpty
               | JUnexpectedCharacter Char
               | JError String
               | JUnknown
  deriving (Show, Eq)