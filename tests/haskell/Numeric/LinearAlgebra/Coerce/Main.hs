{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Prim (coerce)
import Numeric.LinearAlgebra

bad :: Matrix Int -> Matrix Double
bad = coerce
