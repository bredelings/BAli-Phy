module Numeric.Vector () where

import Compiler.Floating
import Numeric.LinearAlgebra.Data

foreign import trcall "Matrix:vectorFloatingUnary" vectorFloatingUnaryNative :: Int -> Vector Double -> Vector Double
foreign import trcall "Matrix:vectorFloatingBinary" vectorFloatingBinaryNative :: Int -> Vector Double -> Vector Double -> Vector Double

vectorFloatingUnary :: Int -> Vector Double -> Vector Double
vectorFloatingUnary operation values =
    overrideVectorSize (vectorSize values) (vectorFloatingUnaryNative operation values)

vectorFloatingBinary :: Int -> Vector Double -> Vector Double -> Vector Double
vectorFloatingBinary operation left right =
    overrideVectorSize (max (vectorSize left) (vectorSize right))
        (vectorFloatingBinaryNative operation left right)

instance Show (Vector a) where
    show value = unpack_cpp_string (showNumericVectorNative (nativeVector value))

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

instance Fractional (Vector Double) where
    (/) = vectorFloatingBinary 0
    recip = vectorFloatingUnary 19

instance Floating (Vector Double) where
    pi = scalar 3.14159265358979323846
    exp = vectorFloatingUnary 0
    sqrt = vectorFloatingUnary 1
    log = vectorFloatingUnary 2
    (**) = vectorFloatingBinary 1
    logBase = vectorFloatingBinary 2
    sin = vectorFloatingUnary 3
    tan = vectorFloatingUnary 4
    cos = vectorFloatingUnary 5
    asin = vectorFloatingUnary 6
    atan = vectorFloatingUnary 7
    acos = vectorFloatingUnary 8
    sinh = vectorFloatingUnary 9
    tanh = vectorFloatingUnary 10
    cosh = vectorFloatingUnary 11
    asinh = vectorFloatingUnary 12
    atanh = vectorFloatingUnary 13
    acosh = vectorFloatingUnary 14
    log1p = vectorFloatingUnary 15
    expm1 = vectorFloatingUnary 16
    log1pexp = vectorFloatingUnary 17
    log1mexp = vectorFloatingUnary 18
