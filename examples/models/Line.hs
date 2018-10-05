module Line where

import Data.ReadFile
import Distributions
import System.Environment

xs = read_file_as_double "xs"

ys = read_file_as_double "ys"

main = do

  b <- sample $ normal 0.0 1.0

  m <- sample $ normal 0.0 1.0

  s <- sample $ gamma 1.0 1.0

  let {y_hat x = m*x + b}

  sequence_ [Observe y (normal y' s) | (x,y) <- zip xs ys, let y' = y_hat x]
  return (Nothing,[
           ("b",(Just b,[])),
           ("m",(Just m,[])),
           ("s",(Just s,[]))])
