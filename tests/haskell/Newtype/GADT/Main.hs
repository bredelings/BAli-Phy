{-# LANGUAGE GADTs #-}
module Main where

newtype Box a where
  Box :: a -> Box a

unBox (Box x) = x

lazyMatch = case (undefined :: Box Int) of Box _ -> 7

ok = unBox (Box 5) == 5
  && map unBox (map Box [1,2]) == [1,2]
  && lazyMatch == 7

main = print (if ok then 1 else 0)
