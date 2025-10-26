module Data.Matrix where

import Foreign.Vector

import Text.Show

-- This is actually a C++ matrix<T> defined in "util/matrix.H"
-- Most of these functions seem to only be defined for matrix<double> = Matrix
data Matrix a

foreign import bpcall "Vector:fromVectors" fromVectors :: EVector (EVector a) -> Matrix a

fromLists xss = fromVectors $ toVector $ map toVector xss

foreign import bpcall "Matrix:" nrows :: Matrix a -> Int
foreign import bpcall "Matrix:" ncols :: Matrix a -> Int
foreign import bpcall "Matrix:" getElem :: Int -> Int -> Matrix Double -> Double

foreign import bpcall "Matrix:transpose" tr :: Matrix a -> Matrix a
foreign import bpcall "Matrix:" scaleMatrix :: a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:elementwise_multiply" (%*%) :: Matrix Double -> Matrix Double -> Matrix Double
foreign import bpcall "Matrix:elementwise_add" (%+%) :: Matrix Double -> Matrix Double -> Matrix Double
foreign import bpcall "Matrix:" zero :: Int -> Int -> Matrix Double
foreign import bpcall "Matrix:" identity :: Int -> Matrix Double
foreign import bpcall "Prelude:show" showMatrix :: Matrix a -> CPPString

instance Show (Matrix a) where
    show x = unpack_cpp_string $ showMatrix x
