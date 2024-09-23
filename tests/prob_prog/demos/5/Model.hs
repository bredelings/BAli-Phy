module Model where

import           Probability

-- sequence of @n points [from, next,....,to] where the distribution
-- of the point after x is (f x).

bridge 2 f from to = do
  observe to (f from)
  return [from,to]

bridge n f from to = do
    next <- prior $ f from
    xs <- bridge (n-1) f next to
    return (from:xs)


-- 20 element brownian bridge from 0 to 4
model = do
    xs <- bridge 20 (\x -> normal x 1) 0 4

    return ["xs" %=% xs]

main logDir = do
  return model
