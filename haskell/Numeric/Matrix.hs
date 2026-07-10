module Numeric.Matrix () where

import Numeric.LinearAlgebra.Data

instance Show (Matrix a) where
    show value = unpack_cpp_string (showMatrix value)

instance (Element a, Num a) => Num (Matrix a) where
    fromInteger value = scalar (fromInteger value)
    negate = mat_negate
    abs = mat_abs
    signum = mat_signum
    (+) = elementwise_add
    (-) = elementwise_sub
    (*) = elementwise_multiply
