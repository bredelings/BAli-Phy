{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import System.IO (print)

main =
    let
        infixr 5 `op`
        op x y = x - y
    in (\op -> print (10 `op` 3 `op` 2)) (-)
