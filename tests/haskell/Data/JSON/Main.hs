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

  -- Check every separately written tuple encoder against its Value encoding.
  -- Existing scalar/vector tests do not exercise tuple field order; generated instances
  -- could replace these arity checks if tuple encoding becomes uniform.
  let t4 = (0,1,2,3) :: (Int,Int,Int,Int)
  putStrLn $ show $ encode t4
  putStrLn $ show $ encode (toJSON t4)
  let t5 = (0,1,2,3,4) :: (Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t5
  putStrLn $ show $ encode (toJSON t5)
  let t6 = (0,1,2,3,4,5) :: (Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t6
  putStrLn $ show $ encode (toJSON t6)
  let t7 = (0,1,2,3,4,5,6) :: (Int,Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t7
  putStrLn $ show $ encode (toJSON t7)
  let t8 = (0,1,2,3,4,5,6,7) :: (Int,Int,Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t8
  putStrLn $ show $ encode (toJSON t8)
  let t9 = (0,1,2,3,4,5,6,7,8) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t9
  putStrLn $ show $ encode (toJSON t9)
  let t10 = (0,1,2,3,4,5,6,7,8,9) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t10
  putStrLn $ show $ encode (toJSON t10)
  let t11 = (0,1,2,3,4,5,6,7,8,9,10) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t11
  putStrLn $ show $ encode (toJSON t11)
  let t12 = (0,1,2,3,4,5,6,7,8,9,10,11) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t12
  putStrLn $ show $ encode (toJSON t12)
  let t13 = (0,1,2,3,4,5,6,7,8,9,10,11,12) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t13
  putStrLn $ show $ encode (toJSON t13)
  let t14 = (0,1,2,3,4,5,6,7,8,9,10,11,12,13) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t14
  putStrLn $ show $ encode (toJSON t14)
  let t15 = (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14) :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)
  putStrLn $ show $ encode t15
  putStrLn $ show $ encode (toJSON t15)
