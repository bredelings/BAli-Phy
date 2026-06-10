{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import System.IO (putStrLn)
import Text.Show (show)

class Foo a where
    foo :: a -> a -> a

instance Foo Int where
    foo = (+)

bar x y = x + (foo y 1)

main = do putStrLn $ show $ bar 2 3
