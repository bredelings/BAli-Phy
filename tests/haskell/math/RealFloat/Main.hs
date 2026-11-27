{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Enum (minBound, maxBound)
import Compiler.Integral ((^))
import Compiler.Fractional
import Compiler.Num
import Compiler.RealFloat
import Data.Eq
import Data.Ord ((<))
import Data.Function (($))
import Data.List ((++))
import Numeric.Log
import System.IO (putStrLn)
import Text.Show (show)

x = 2_002.0_0e1_0 :: Double

y = 2 :: Log Double

-- Small exact arithmetic alone cannot expose premature conversion of a large significand
-- or double rounding at the normal and subnormal boundaries.
encode :: Integer -> Int -> Double
encode = encodeFloat

two :: Int -> Integer
two n = 2 ^ n

roundTrips value = case decodeFloat value of
    (m, e) -> encode m e == value

main = do
  putStrLn $ "x = " ++ (show x)
  putStrLn $ "floatRadix x = " ++ (show $ floatRadix x)
  putStrLn $ "floatDigits x = " ++ (show $ floatDigits x)
  putStrLn $ "floatRange x = " ++ (show $ floatRange x)
  putStrLn $ "decodeFloat x = " ++ (show $ decodeFloat x)
  putStrLn $ "encodeFloat 2 3 = " ++ (show $ (encodeFloat 2 3 :: Double))
  putStrLn $ "significand x = " ++ (show $ significand x)
  putStrLn $ "exponent x = " ++ (show $ exponent x)
  putStrLn $ "scaleFloat 2 x = " ++ (show $ scaleFloat 2 x)
  putStrLn $ "isNaN x = " ++ (show $ isNaN x)
  putStrLn $ "isInfinite x = " ++ (show $ isInfinite x)
  putStrLn $ "isDenormalized x = " ++ (show $ isDenormalized x)
  putStrLn $ "isNegativeZero x = " ++ (show $ isNegativeZero x)
  putStrLn $ show -2#
  putStrLn $ show -4#
  putStrLn $ show -4_4#
  let negativeHalf = encode (negate (two 54)) (-1129)
      least = encode 1 (-1074)
      leastNormal = encode 1 (-1022)
      normalMidpoint = 4 * (two 53 - 1)
      overflowMidpoint = two 54 - 1
      greatest = encode (two 53 - 1) 971
  putStrLn $ "large significand = " ++ show (encode (two 1100) (-1100) == 1 && encode (negate (two 100)) (-100) == -1)
  putStrLn $ "even ties = " ++ show (encode (two 53 + 1) 0 == 9007199254740992
                && encode (two 53 + 3) 0 == 9007199254740996)
  putStrLn $ "subnormal rounding = " ++ show (encode (two 53 + 1) (-1128) == least
                && encode (two 54) (-1129) == 0
                && negativeHalf == 0 && 1 / negativeHalf < 0)
  putStrLn $ "normal boundary = " ++ show (encode normalMidpoint (-1077) == leastNormal
                && encode (normalMidpoint - 1) (-1077) == leastNormal - least)
  putStrLn $ "overflow midpoint = " ++ show (encode (2 * overflowMidpoint - 1) 969 == greatest
                && isInfinite (encode overflowMidpoint 970))
  putStrLn $ "extreme exponents = " ++ show (encode (two 100) minBound == 0 && isInfinite (encode (two 100) maxBound)
                && encode 0 maxBound == 0)
  putStrLn $ "finite round trips = " ++ show (roundTrips least && roundTrips leastNormal && roundTrips greatest)
