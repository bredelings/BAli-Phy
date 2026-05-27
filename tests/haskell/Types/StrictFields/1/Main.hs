{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.IO (IO(IO))

data A = A
data Box = Box !A

done = IO (\s -> (s, ()))

main = case Box (error "strict field forced") of
  Box _ -> done
