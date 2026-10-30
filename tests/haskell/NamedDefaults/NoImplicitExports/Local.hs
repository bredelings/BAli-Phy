{-# LANGUAGE NamedDefaults, NoImplicitPrelude #-}
module Local where
import Compiler.Num
import Data.Bool (Bool)
class Choose a where
    choose :: a -> Int
instance Choose Bool where
    choose _ = 1
default Choose (Bool)
