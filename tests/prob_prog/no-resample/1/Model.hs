module Model where

import           Probability

my_iid n dist =
    let body = do
            x  <- dist
            xs <- my_iid (n - 1) dist
            return (x : xs)
    in  if n == 0 then return [] else body

model x = do
    n  <- min 100 <$> sample (geometric 0.25)
    xs <- lazy $ my_iid n (sample $ normal 0 1)
    let total = sum xs
    let loggers = ["n" %=% n, "xs" %=% xs]
    observe x $ normal total 1
    return loggers

main = do
  return $ model 20
