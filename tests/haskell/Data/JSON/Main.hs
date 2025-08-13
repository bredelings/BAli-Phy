{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Semigroup
import Data.JSON    
    
data Person = Person { name :: Text, age :: Int}

instance ToJSON Person where
    toJSON (Person name age) = object [ "name" .= name, "age" .= age]

    toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)

main = do
  putStrLn $ show $ encode (Person "Joe" 12)
