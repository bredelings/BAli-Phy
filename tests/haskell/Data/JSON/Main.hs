{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import Compiler.Num
import Compiler.Base (String)
import Data.Text (Text)
import Data.Function (($))
import Data.Semigroup
import Data.JSON    
import qualified Data.Map as Map
import System.IO (putStrLn)
import Text.Show (show)
    
data Person = Person { name :: Text, age :: Int}

instance ToJSON Person where
    toJSON (Person name age) = object [ "name" .= name, "age" .= age]

    toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)

main = do
  putStrLn $ show $ encode (Person "Joe" 12)

  -- Keep ordered association lists distinct from JSON objects; this test becomes
  -- obsolete if the language gains separate standard types for both concepts.
  let entries = [("A", 1), ("C", 2)] :: [(String, Int)]
  putStrLn $ show $ encode entries
  putStrLn $ show $ encode (Map.fromList entries)
