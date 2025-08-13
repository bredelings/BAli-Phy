{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Text (Text)

import Data.JSON    
    
data Person = Person { name :: Text, age :: Int}

instance ToJSON Person where
    toJSON (Person name age) = object [ "name" .= name, "age" .= age]

main = do
  putStrLn $ show $ encode (Person "Joe" 12)
