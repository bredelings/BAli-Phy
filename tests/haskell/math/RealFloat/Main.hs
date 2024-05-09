import Numeric.Log

x = 2_002.0_0e1_0 :: Double

y = 2 :: Log Double

main = do
  putStrLn $ "x = " ++ (show x)
  putStrLn $ "floatRadix x = " ++ (show $ floatRadix x)
  putStrLn $ "floatDigits x = " ++ (show $ floatDigits x)
  putStrLn $ "floatRange x = " ++ (show $ floatRange x)
  putStrLn $ "decodeFloat x = " ++ (show $ decodeFloat x)
  putStrLn $ "encodeFloat 2 3 = " ++ (show $ encodeFloat 2 3)
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
