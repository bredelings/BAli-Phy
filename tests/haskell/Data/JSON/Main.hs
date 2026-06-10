{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import Compiler.Num
import Data.Text (Text)
import Data.Function (($))
import Data.Semigroup
import Data.JSON    
import System.IO (putStrLn)
import Text.Show (show)
    
data Person = Person { name :: Text, age :: Int}

instance ToJSON Person where
    toJSON (Person name age) = object [ "name" .= name, "age" .= age]

    toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)

main = do
  putStrLn $ show $ encode (Person "Joe" 12)
