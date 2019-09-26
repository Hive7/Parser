module JSONTypes where

data JSON = JString String
          | JNumber Double
          | JBool Bool
          | JNull
          | JArray [JSON]
          | JObject [(String, JSON)]
  deriving (Show, Eq, Ord)