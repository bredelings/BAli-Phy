module Test where

import Compiler.Prim
import Control.DeepSeq
import Data.Text.Display

-- data T a = T !(a -> !a) !Int b
data T a = T !(a -> a) Int !b

class C a where
    type K a
    type K a = ()
    type K a a a = a
    asdf :: a -> Int

instance Show a => C a where
    asdf x  = x

f x y = seq x y    

main = putStrLn $ show $ f 1 2
