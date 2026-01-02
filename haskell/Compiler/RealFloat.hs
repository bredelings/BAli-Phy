{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.RealFloat where

import Compiler.Error
import Compiler.Num
import Compiler.RealFrac
import Compiler.Floating
import Data.Bool
import Data.Eq
import Data.Ord
import Data.Tuple
import Data.Function
import Foreign.Pair

class (RealFrac a, Floating a) => RealFloat a where
    floatRadix :: a -> Integer
    floatDigits :: a -> Int
    floatRange :: a -> (Int, Int)
    decodeFloat :: a -> (Integer, Int)
    encodeFloat :: Integer -> Int -> a
    exponent :: a -> Int
    significand :: a -> a
    scaleFloat :: Int -> a -> a
    isNaN :: a -> Bool
    isInfinite :: a -> Bool
    isDenormalized :: a -> Bool
    isNegativeZero :: a -> Bool
    isIEEE :: a -> Bool
    atan2 :: a -> a -> a

instance RealFloat Double where
    floatRadix _  = 2
    floatDigits _ = 53
    floatRange  _ = (-1021,1024)
    decodeFloat x = decodeDouble x
    encodeFloat sig exp = encodeDouble sig exp
    exponent x    = case decodeFloat x of (m,n) -> if m == 0 then 0 else n + floatDigits x
    significand x = case decodeFloat x of (m,_) -> encodeFloat m (negate (floatDigits x))
    scaleFloat 0 x = x
    scaleFloat k x
        | isFix = x
        | otherwise = case decodeFloat x of
                        (m,n) -> encodeFloat m (n + clamp bd k)
                      where bd = snd (floatRange x) - fst (floatRange x) + 4*floatDigits x
                            isFix = x == 0 || (not $ isDoubleFinite x)
                            clamp :: Int -> Int -> Int
                            clamp bd k = max (-bd) (min bd k)

    isNaN x  = isDoubleNaN x
    isInfinite x = isDoubleInfinite x
    isDenormalized x = isDoubleDenormalized x
    isNegativeZero x = isDoubleNegativeZero x
    isIEEE _ = True
    atan2 x y = atan2_double x y


foreign import ecall "Real:" isDoubleNaN :: Double -> Bool
foreign import ecall "Real:" isDoubleInfinite :: Double -> Bool
foreign import ecall "Real:" isDoubleFinite :: Double -> Bool
foreign import ecall "Real:" isDoubleDenormalized :: Double -> Bool
foreign import ecall "Real:" isDoubleNegativeZero :: Double -> Bool
foreign import ecall "Real:" atan2_double :: Double -> Double -> Double
foreign import ecall "Real:" decodeDoubleRaw :: Double -> EPair Integer Int
foreign import ecall "Real:" encodeDouble :: Integer -> Int -> Double

decodeDouble = pair_from_c . decodeDoubleRaw

