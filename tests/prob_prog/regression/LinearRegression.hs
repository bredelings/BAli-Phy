module Line where

import Data.ReadFile
import Distributions
import System.Environment

xs = read_file_as_double "xs"

ys = read_file_as_double "ys"

main = do

  b <- sample $ normal 0.0 1.0

  a <- sample $ normal 0.0 1.0

  s <- sample $ exponential 1.0

  let f x = b*x + a

  sequence_ [observe (normal mu_y s) y | (x,y) <- zip xs ys, let mu_y = f x]

  return $ log_all [b %% "b", a %% "a", s %% "s"]
