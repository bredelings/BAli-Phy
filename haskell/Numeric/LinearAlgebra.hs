module Numeric.LinearAlgebra where

import Foreign.Vector

import qualified Data.Foldable as F    

import Text.Show

-- Dense native matrices backed by the C++ matrix<T> in "util/matrix.H".
-- Only element representations supported by the runtime can inhabit Matrix.
-- The foreign-import "Matrix:" prefixes name the C++ builtin plugin.
type role Matrix nominal
data Matrix a

foreign import ecall "Matrix:" nrows :: Matrix a -> Int
foreign import ecall "Matrix:" ncols :: Matrix a -> Int

foreign import ecall "Prelude:show" showMatrix :: Matrix a -> CPPString
-- prettyMatrix :: Show a => Matrix a -> String
-- forceMatrix :: Matrix a -> Matrix a                                         
-- matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a

-- rowVector :: Vector a -> Matrix a
-- colVector :: Vector a -> Matrix a
-- diagonalList :: Int -> a -> [a] -> Matrix a
-- diagonal :: a -> Vector a -> Matrix a
-- permMatrix :: Num a => Int -> Int -> Int -> Matrix a

infixl 4 ><

-- Element selects the native representation used when constructing a matrix.
-- Instances are deliberately limited to representations supported by C++.
class Element a where
    (><) :: Int -> Int -> [a] -> Matrix a

foreign import bpcall "Matrix:intMatrixFromList" intMatrixFromList :: Int -> Int -> [Int] -> Matrix Int
foreign import bpcall "Matrix:doubleMatrixFromList" doubleMatrixFromList :: Int -> Int -> [Double] -> Matrix Double

instance Element Int where
    (><) = intMatrixFromList

instance Element Double where
    (><) = doubleMatrixFromList

-- Construct a rectangular matrix from rows, expanding singleton rows to the
-- longest row and rejecting all other incompatible row lengths.
fromLists :: Element a => [[a]] -> Matrix a
fromLists [] = (0 >< 0) []
fromLists xss
    | all compatible lengths = (length xss >< columns) (concatMap expand xss)
    | otherwise = error "Numeric.LinearAlgebra.fromLists: rows have incompatible lengths"
  where
    lengths = map length xss
    columns = maximum lengths
    compatible n = n == columns || n == 1
    expand [x] = replicate columns x
    expand xs = xs

zero :: (Element a, Num a) => Int -> Int -> Matrix a
zero rows columns = (rows >< columns) (replicate (rows * columns) 0)

identity :: (Element a, Num a) => Int -> Matrix a
identity size = (size >< size) [if i == j then 1 else 0 | i <- [0..size-1], j <- [0..size-1]]

-- NOTE: Flatten through EVector until Numeric.LinearAlgebra has a native
-- numerical vector representation.
foreign import bpcall "Matrix:" matrixToVector :: Matrix a -> EVector a
toList :: Matrix a -> [a]
toList = vectorToList . matrixToVector

-- Split a row-major list into the requested number of rows, including empty
-- rows when the matrix has zero columns.
splitRows :: Int -> Int -> [a] -> [[a]]
splitRows 0 _ _ = []
splitRows rows columns xs = take columns xs : splitRows (rows-1) columns (drop columns xs)

toLists :: Matrix a -> [[a]]
toLists matrix = splitRows (nrows matrix) (ncols matrix) (toList matrix)

foreign import ecall "Matrix:" getElem :: Int -> Int -> Matrix a -> a
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

instance (Element a, Num a) => Num (Matrix a) where
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
