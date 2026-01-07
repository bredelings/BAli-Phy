module Data.Matrix where

import Foreign.Vector

import qualified Data.Foldable as F    

import Text.Show

-- This is actually a C++ matrix<T> defined in "util/matrix.H"
--    Most of these functions seem to only be defined for matrix<double> = Matrix

-- It looks like the haskell Data.Matrix is more like a lazy array with index type (i,j).
data Matrix a

foreign import ecall "Matrix:" nrows :: Matrix a -> Int
foreign import ecall "Matrix:" ncols :: Matrix a -> Int

foreign import ecall "Prelude:show" showMatrix :: Matrix a -> CPPString
-- prettyMatrix :: Show a => Matrix a -> String
-- forceMatrix :: Matrix a -> Matrix a                                         
-- matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a

-- rowVector :: Vector a -> Matrix a
-- colVector :: Vector a -> Matrix a
-- zero :: Num a -> Int -> Int -> Matrix a
foreign import bpcall "Matrix:" zero :: Int -> Int -> Matrix Double
-- identity :: Num a => Int -> Matrix a
foreign import bpcall "Matrix:" identity :: Int -> Matrix Double
-- diagonalList :: Int -> a -> [a] -> Matrix a
-- diagonal :: a -> Vector a -> Matrix a
-- permMatrix :: Num a => Int -> Int -> Int -> Matrix a

foreign import bpcall "Vector:" vectorToMatrix :: Int -> Int -> EVector a -> Matrix a
fromList :: Int -> Int -> [a] -> Matrix a
fromList i j xs = vectorToMatrix i j (listToVector xs)            

foreign import bpcall "Vector:fromVectors" fromVectors :: EVector (EVector a) -> Matrix a
fromLists :: [[a]] -> Matrix a
fromLists xss = fromVectors $ toVector $ map toVector xss

foreign import bpcall "Vector:" matrixToVector :: Matrix a -> EVector a
-- toList :: Matrix a -> [a]
toList = vectorToList . matrixToVector
--toLists :: Matrix a -> [[a]]

-- getElem :: Int -> Itt -> Matrix a -> a
foreign import ecall "Matrix:" getElem :: Int -> Int -> Matrix Double -> Double
-- unsafeGet :: Int -> Int -> Matrix a -> a           
-- safeGet :: Int -> Int -> Matrix a -> Maybe a
-- safeSet :: a -> (Int,Int) -> Matrix a -> Maybe (Matrix a)
-- getRow :: Int -> Matrix a -> Vector a
-- safeGetRow :: Int -> Matrix a -> Maybe (Vector a)
-- getCol :: Int -> Matrix a -> Vector a
-- safeGetCol :: Int -> Matrix a -> Maybe (Vector a)
-- getDiag :: Matrix a -> Vector a
-- getMatrixAsVector :: Matrix a -> Vector a

-- setElem :: a -> (Int, Int) -> Matrix a -> Matrix a
-- unsafeSet :: a -> (Int, Int) -> Matrix a -> Matrix a
-- transpose :: Matrix a -> Matrix a
foreign import bpcall "Matrix:" transpose :: Matrix a -> Matrix a
-- setSize :: a -> Int -> Int -> Matrix a -> Matrix a
-- extendTo :: a -> Int -> Int -> Matrix a -> Matrix a
-- inverse :: (Fractional a, Eq a) => Matrix a -> Either String (Matrix a)
-- rref :: (Fractional a, Eq a) => Matrix a -> Either String (Matrix a)
-- mapRow
-- mapCol
-- mapPos

-- submatrix
-- minorMatrix
-- splitBlocks
-- (<|>) :: Matrix a -> Matrix a -> Matrix a  = horizontally join two matrices
-- (<->) :: Matrix a -> Matrix a -> Matrix a  = vertically join two matrices
-- joinBlocks :: (Matrix a, Matrix a, Matrix a, Matrix a) -> Matrix a

-- elementwise :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
-- elementwiseUnsafe

-- multStd :: Num a => Matrix a -> Matrix a -> Matrix a
-- multStd2 :: Num a => Matrix a -> Matrix a -> Matrix a
-- multStrassen :: Num a => Matrix a -> Matrix a -> Matrix a
-- multStrassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a 


-- scaleMatrix :: Num a => a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" scaleMatrix :: a -> Matrix a -> Matrix a
-- scaleRow :: Num a => a -> Int -> Matrix a -> Matrix a 
-- combineRows :: Num a => Int -> a -> Int -> Matrix a -> Matrix a 
-- switchRows :: Int -> Int -> Matrix a -> Matrix a
-- switchCols:: Int -> Int -> Matrix a -> Matrix a
-- luDecomp :: (Ord a, Fractional a) => Matrix a -> Maybe (Matrix a, Matrix a, Matrix a, a)
-- luDecompUnsafe :: (Ord a, Fractional a) => Matrix a -> (Matrix a, Matrix a, Matrix a, a)
-- luDecomp' :: (Ord a, Fractional a) => Matrix a -> Maybe (Matrix a, Matrix a, Matrix a, Matrix a, a, a) 
-- luDecompUnsafe' :: (Ord a, Fractional a) => Matrix a -> (Matrix a, Matrix a, Matrix a, Matrix a, a, a)
-- cholDecomp :: Floating a => Matrix a -> Matrix a

-- trace :: Num a => Matrix a -> a 
-- diagProd :: Num a => Matrix a -> a 

-- detLaplace :: Num a => Matrix a -> a 
-- detLU :: (Ord a, Fractional a) => Matrix a -> a 
-- flatten :: Matrix (Matrix a) -> Matrix a 

(%*%) = elementwise_multiply
                                              
instance Show (Matrix a) where
    show x = unpack_cpp_string $ showMatrix x

instance Num a => Num (Matrix a) where
    fromInteger x = fromLists [[fromInteger x]]

    negate = mat_negate
    abs = mat_abs
    signum = mat_signum

    (+) = elementwise_add

    (-) = elementwise_sub

    (*) = mat_mult


instance F.Foldable Matrix where
    toList = toList


foreign import bpcall "Matrix:" elementwise_multiply :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" elementwise_add :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" elementwise_sub :: Matrix a -> Matrix a -> Matrix a

foreign import bpcall "Matrix:" mat_mult :: Matrix a -> Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_abs :: Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_negate :: Matrix a -> Matrix a
foreign import bpcall "Matrix:" mat_signum :: Matrix a -> Matrix a

