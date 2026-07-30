{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Control.Monad (return)
import System.IO (print)

infixr 5 `op`
op x y = x - y

main = do
    op <- return (-)
    print (10 `op` 3 `op` 2)
