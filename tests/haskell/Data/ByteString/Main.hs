module Main where

import Data.ByteString as B
import Data.Monoid
import Data.Word

main = do
  let bs = B.pack [0, 65, 128, 255]
  print (B.length bs == 4)
  print (B.unpack bs == [0, 65, 128, 255])
  print (B.index bs 0 == 0)
  print (B.index bs 3 == 255)
  print (B.unpack (B.take 2 bs) == [0, 65])
  print (B.unpack (B.drop 2 bs) == [128, 255])
  print (B.unpack (B.take (-1) bs) == [])
  print (B.unpack (B.drop (-1) bs) == [0, 65, 128, 255])
  print (B.unpack (B.append (B.pack [1, 2]) (B.pack [3, 4])) == [1, 2, 3, 4])
  print (B.pack [256, -1] == B.pack [0, 255])
  print ((B.pack [1, 2] <> B.pack [3]) == B.pack [1, 2, 3])
  print (mempty == (B.empty :: ByteString))
