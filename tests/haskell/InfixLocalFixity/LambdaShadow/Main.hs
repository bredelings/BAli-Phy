{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import System.IO (print)

infixr 5 `op`
op x y = x - y

main = (\op -> print (10 `op` 3 `op` 2)) (-)
