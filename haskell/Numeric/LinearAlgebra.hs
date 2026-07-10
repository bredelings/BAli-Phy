module Numeric.LinearAlgebra
    ( R
    , I
    , Vector
    , Matrix
    , Element(..)
    , IndexOf
    , Container(..)
    , vector
    , matrix
    , range
    , idxs
    , toList
    , rows
    , cols
    , fromLists
    , toLists
    , row
    , col
    , flatten
    , reshape
    , asRow
    , asColumn
    , fromRows
    , toRows
    , fromColumns
    , toColumns
    , build
    , ident
    , diag
    , diagl
    , diagRect
    , takeDiag
    , linspace
    , subVector
    , takesV
    , vjoin
    , subMatrix
    , takeRows
    , dropRows
    , takeColumns
    , dropColumns
    , flipud
    , fliprl
    , Extractor(..)
    , (??)
    , fromBlocks
    , (|||)
    , (===)
    , diagBlock
    , repmat
    , toBlocks
    , toBlocksEvery
    , cmap
    , prodElements
    , minElement
    , maxElement
    , minIndex
    , maxIndex
    , find
    , sortVector
    , sortIndex
    , conj
    , cmod
    , tr
    , scale
    , dot
    , (<.>)
    , (#>)
    , (<#)
    , (<>)
    , outer
    , optimiseMult
    , det
    , inv
    , linearSolve
    , linearSolveLS
    , chol
    , expm
    , eigSH
    , eigenvaluesSH
    , svd
    , thinSVD
    , singularValues
    , qr
    , thinQR
    ) where

import Foreign.Maybe (CMaybe, fromCMaybe)
import Foreign.Pair (EPair, pair_from_c)
import Prelude hiding ((<>), find)
import Numeric.LinearAlgebra.Data
import Numeric.Matrix (optimiseMult)
import Numeric.Vector ()

infixr 8 <>

dot :: Element a => Vector a -> Vector a -> a
dot = dotNative

(<.>) :: Element a => Vector a -> Vector a -> a
(<.>) = dot

(#>) :: Element a => Matrix a -> Vector a -> Vector a
(#>) = matrixVectorNative

(<#) :: Element a => Vector a -> Matrix a -> Vector a
(<#) = vectorMatrixNative

(<>) :: Element a => Matrix a -> Matrix a -> Matrix a
(<>) = matrixProduct

outer :: Element a => Vector a -> Vector a -> Matrix a
outer = outerNative

foreign import bpcall "Matrix:" detNative :: Matrix Double -> Double
foreign import bpcall "Matrix:" invNative :: Matrix Double -> Matrix Double
foreign import bpcall "Matrix:" linearSolveNative :: Matrix Double -> Matrix Double -> CMaybe (Matrix Double)
foreign import bpcall "Matrix:" linearSolveLSNative :: Matrix Double -> Matrix Double -> Matrix Double
foreign import bpcall "Matrix:" cholNative :: Matrix Double -> Matrix Double
foreign import bpcall "Matrix:" expmNative :: Matrix Double -> Matrix Double

det :: Matrix Double -> Double
det = detNative

inv :: Matrix Double -> Matrix Double
inv = invNative

linearSolve :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
linearSolve coefficients rhs = fromCMaybe (linearSolveNative coefficients rhs)

linearSolveLS :: Matrix Double -> Matrix Double -> Matrix Double
linearSolveLS = linearSolveLSNative

chol :: Matrix Double -> Matrix Double
chol = cholNative

expm :: Matrix Double -> Matrix Double
expm = expmNative

foreign import bpcall "Matrix:" eigSHNative :: Matrix Double -> EPair (Vector Double) (Matrix Double)
foreign import bpcall "Matrix:" eigenvaluesSHNative :: Matrix Double -> Vector Double
foreign import bpcall "Matrix:" svdNative :: Int -> Matrix Double -> EPair (Matrix Double) (EPair (Vector Double) (Matrix Double))
foreign import bpcall "Matrix:" singularValuesNative :: Matrix Double -> Vector Double
foreign import bpcall "Matrix:" qrNative :: Int -> Matrix Double -> EPair (Matrix Double) (Matrix Double)

eigSH :: Matrix Double -> (Vector Double, Matrix Double)
eigSH = pair_from_c . eigSHNative

eigenvaluesSH :: Matrix Double -> Vector Double
eigenvaluesSH = eigenvaluesSHNative

-- Unpack one native SVD computation into the public hmatrix-style triple.
svdResult :: EPair (Matrix Double) (EPair (Vector Double) (Matrix Double))
          -> (Matrix Double, Vector Double, Matrix Double)
svdResult decomposition = (u, singular, v)
  where
    (u, rest) = pair_from_c decomposition
    (singular, v) = pair_from_c rest

svd :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
svd = svdResult . svdNative 0

thinSVD :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
thinSVD = svdResult . svdNative 1

singularValues :: Matrix Double -> Vector Double
singularValues = singularValuesNative

qr :: Matrix Double -> (Matrix Double, Matrix Double)
qr = pair_from_c . qrNative 0

thinQR :: Matrix Double -> (Matrix Double, Matrix Double)
thinQR = pair_from_c . qrNative 1
