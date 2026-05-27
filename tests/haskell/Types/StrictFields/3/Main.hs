{-# LANGUAGE GADTs, NoImplicitPrelude #-}

import Compiler.Error (error)
import Compiler.IO (IO(IO))

data A = A
data GBox a where
  GBox :: !A -> GBox A

done = IO (\s -> (s, ()))

main = case GBox (error "gadt strict field forced") of
  GBox _ -> done
