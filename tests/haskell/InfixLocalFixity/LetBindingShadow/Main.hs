{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import System.IO (print)

infixr 5 `op`
op x y = x - y

main =
    let op x y = x - y
    in print (10 `op` 3 `op` 2)
