module Main where

newtype Age = Age Int deriving Eq deriving Ord
newtype Box a = Box a deriving Eq
newtype UserId = UserId { userId :: Int } deriving Eq

unAge (Age x) = x

lazyMatch = case (undefined :: Age) of Age _ -> 7

nestedMatch (Box (Just x)) = x
nestedMatch (Box Nothing) = 0

lazyList = [1 | Box _ <- [undefined :: Box Int]]

refutableList = [x | Box (Just x) <- [Box Nothing, Box (Just 4)]]

ok = Age 1 < Age 2
  && Age 3 == Age 3
  && unAge (Age 5) == 5
  && map Age [1,2] == [Age 1, Age 2]
  && lazyMatch == 7
  && nestedMatch (Box (Just 9)) == 9
  && lazyList == [1]
  && refutableList == [4]
  && userId ((UserId 3) { userId = 11 }) == 11

main = print (if ok then 1 else 0)
