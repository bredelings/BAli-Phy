{-# LANGUAGE NoImplicitPrelude #-}

data family F a

data instance F [a] = MkF a

mk :: Int -> F [Int]
mk x = MkF x

un :: F [Int] -> Int
un (MkF x) = x
