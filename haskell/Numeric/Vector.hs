module Numeric.Vector () where

import Numeric.LinearAlgebra.Data

instance Show (Vector a) where
    show value = unpack_cpp_string (showNumericVector value)

instance Element a => Eq (Vector a) where
    (==) = vectorEqual

instance (Element a, Num a) => Num (Vector a) where
    fromInteger value = scalar (fromInteger value)
    negate = vector_negate
    abs = vector_abs
    signum = vector_signum
    (+) = vector_elementwise_add
    (-) = vector_elementwise_sub
    (*) = vector_elementwise_multiply
