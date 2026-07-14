{-# LANGUAGE TypeFamilies #-}
module SModel.Likelihood.CLV where

import Compiler.FFI.Import (COutput(..))
import Compiler.FFI.Runtime (RuntimeValue)

data CondLikes

instance RuntimeValue CondLikes

instance COutput CondLikes where
    type COutputType CondLikes = CondLikes
    fromCOutput value = value
