{-# LANGUAGE NoImplicitPrelude #-}
module Base where
import Compiler.Num
import Data.Bool (Bool)
class Choose a where
    choose :: a -> Int
instance Choose Bool where
    choose _ = 1
instance Choose Char where
    choose _ = 2
class Other a where
    other :: a -> Int
instance Other Bool where
    other _ = 3
