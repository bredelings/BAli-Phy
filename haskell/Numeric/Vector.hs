module Numeric.Vector () where

import Numeric.LinearAlgebra.Data

instance Show (Vector a) where
    show value = unpack_cpp_string (showNumericVector value)

instance Element a => Eq (Vector a) where
    (==) = vectorEqual
