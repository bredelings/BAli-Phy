{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.IO (IO(IO), unsafePerformIO) where

import Compiler.Num

type RealWorld = Int

data IO a = IO (RealWorld -> (RealWorld, a))

runIO (IO f) = f

unsafePerformIO :: IO c -> c
unsafePerformIO f = let (s,x) = runIO f 0#
                    in x
