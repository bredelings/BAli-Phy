{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

import Compiler.Num
import Compiler.Base (String)
import Data.Text (Text)
import Data.Function (($))
import Data.Semigroup
import Data.JSON    
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (removeMinusOnes)
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

  -- Whole-vector encoding must preserve negatives and slices; filtering removes only -1.
  -- Scalar JSON tests cannot catch native view mistakes; this group is obsolete if that path disappears.
  let values = U.slice 1 4 (U.fromList [99,-1,2,-2,3,99] :: U.Vector Int)
  putStrLn $ show $ encode values
  putStrLn $ show $ encode (toJSON values)
  putStrLn $ show $ encode (removeMinusOnes values)
  putStrLn $ show $ encode (removeMinusOnes (U.empty :: U.Vector Int))
  putStrLn $ show $ encode (removeMinusOnes (U.fromList [-1,-1]))
  putStrLn $ show $ encode (removeMinusOnes (U.fromList [2,3]))
