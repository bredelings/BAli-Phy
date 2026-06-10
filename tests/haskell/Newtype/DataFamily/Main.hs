module Main where

data family F a

newtype instance F Int = MkF Int

unF (MkF x) = x

lazyMatch = case (undefined :: F Int) of MkF _ -> 7

ok = unF (MkF 5) == 5
  && map unF (map MkF [1,2]) == [1,2]
  && lazyMatch == 7

main = print (if ok then 1 else 0)
