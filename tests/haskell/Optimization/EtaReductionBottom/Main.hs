{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Base (seq)
import Compiler.Error (error)
import System.IO (putStrLn)

bottom = error "eta reduction forced bottom"

wrapped = \x -> bottom x

main = seq wrapped (putStrLn "ok")
