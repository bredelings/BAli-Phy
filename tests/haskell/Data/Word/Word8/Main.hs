module Main where

import Data.Word

main = do
  print (word8ToInt (0 :: Word8) == 0)
  print (word8ToInt (255 :: Word8) == 255)
  print (word8ToInt (256 :: Word8) == 0)
  print (word8ToInt ((-1) :: Word8) == 255)
  print (word8ToInt (integerToWord8 (-1)) == 255)
  print (word8ToInt ((255 :: Word8) + 1) == 0)
  print (word8ToInt ((0 :: Word8) - 1) == 255)
  print (word8ToInt ((16 :: Word8) * 16) == 0)
  print (show (255 :: Word8) == "255")
