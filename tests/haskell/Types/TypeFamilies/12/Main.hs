{-# LANGUAGE NoImplicitPrelude #-}

type family Same a b where
    Same a a = Double
    Same a b = Char

sameTypes :: Same Int Int -> Double
sameTypes x = x

differentTypes :: Same Int Char -> Char
differentTypes x = x
