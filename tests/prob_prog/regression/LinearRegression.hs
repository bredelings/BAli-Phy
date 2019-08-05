module Line where

import Probability
import Data.ReadFile
import System.Environment

xs = read_file_as_double "xs"

ys = read_file_as_double "ys"

sample_model = do

  b <- sample $ normal 0.0 1.0

  a <- sample $ normal 0.0 1.0

  s <- sample $ exponential 1.0

  return (a,b,s)

main = do

  (a,b,s) <- random $ sample_model
  
  let f x = b*x + a

  sequence_ [observe (normal mu_y s) y | (x,y) <- zip xs ys, let mu_y = f x]

  return $ log_all [b %% "b", a %% "a", s %% "s"]
