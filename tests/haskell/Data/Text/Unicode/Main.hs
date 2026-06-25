module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (print)

main = do
  let a = '\256'
  let t = T.pack ['A', a, 'B']
  print (T.length (T.singleton a) == 1)
  print (T.length t == 3)
  print (T.unpack t == ['A', a, 'B'])
  print (T.head t == 'A')
  print (T.last t == 'B')
  print (T.unpack (T.tail t) == [a, 'B'])
  print (T.unpack (T.init t) == ['A', a])
  case T.uncons t of
    Just (c, rest) -> print (c == 'A' && T.unpack rest == [a, 'B'])
    Nothing -> print False
  let appended = T.append (T.singleton a) (T.singleton 'C')
  print (T.length appended == 2)
  print (T.unpack appended == [a, 'C'])
  print (T.pack ['A'] < T.pack [a])
  print (T.pack [a] == T.pack [a])
  fileText <- TIO.readFile "input.txt"
  print (T.length fileText == 4)
  print (T.unpack fileText == ['A', a, 'B', '\n'])
