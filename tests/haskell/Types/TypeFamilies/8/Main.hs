{-# LANGUAGE NoImplicitPrelude #-}

type family F a
type instance F [a] = [a]
type instance F [Int] = [Int]

compatibleOverlap :: F [Int] -> [Int]
compatibleOverlap x = x
