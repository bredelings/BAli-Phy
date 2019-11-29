module Line where

import Data.ReadFile
import Probability
import System.Environment

xs = read_file_as_double "xs"

ys = read_file_as_double "ys"

model = do
  a <- normal 0.0 1.0
  b <- normal 0.0 1.0
  s <- exponential 1.0
  return (a, b, s)

main = do

  let (a,b,s) = random $ model

  let f x = b*x + a

  sequence_ [observe (normal mu_y s) y | (x,y) <- zip xs ys, let mu_y = f x]

  return $ log_all ["b" %=% b, "a" %=% a, "s" %=% s]
