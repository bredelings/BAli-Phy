module Main where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main = do
  let t = T.pack ['A', '\256', 'B']
  print (B.unpack (TE.encodeUtf8 t) == [65, 196, 128, 66])
  print (TE.decodeUtf8 (B.pack [65, 196, 128, 66]) == t)
  print (TE.decodeUtf8 (B.pack [0, 65]) == T.pack ['\0', 'A'])
  print (TE.encodeUtf8 T.empty == B.empty)
  print (TE.decodeUtf8 B.empty == T.empty)
