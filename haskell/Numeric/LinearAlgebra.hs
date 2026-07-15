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
    , overrideVectorSize
    , overrideMatrixDims
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

import Foreign.Maybe ()
import Prelude hiding ((<>), find)
import Numeric.LinearAlgebra.Data
import Numeric.Matrix (optimiseMult)
import Numeric.Vector ()

infixr 8 <>

dot :: Element a => Vector a -> Vector a -> a
dot = dotVector

(<.>) :: Element a => Vector a -> Vector a -> a
(<.>) = dot

(#>) :: Element a => Matrix a -> Vector a -> Vector a
(#>) = matrixVectorProduct

(<#) :: Element a => Vector a -> Matrix a -> Vector a
(<#) = vectorMatrixProduct

(<>) :: Element a => Matrix a -> Matrix a -> Matrix a
(<>) = matrixProduct

outer :: Element a => Vector a -> Vector a -> Matrix a
outer = outerProduct

foreign import trcall "Matrix:detNative" detNative :: Matrix Double -> Double
foreign import trcall "Matrix:invNative" invNative :: Matrix Double -> Matrix Double
foreign import trcall "Matrix:linearSolveNative" linearSolveNative :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
foreign import trcall "Matrix:linearSolveLSNative" linearSolveLSNative :: Matrix Double -> Matrix Double -> Matrix Double
foreign import trcall "Matrix:cholNative" cholNative :: Matrix Double -> Matrix Double
foreign import trcall "Matrix:expmNative" expmNative :: Matrix Double -> Matrix Double

det :: Matrix Double -> Double
det = detNative

inv :: Matrix Double -> Matrix Double
inv matrix = overrideMatrixDims (rows matrix) (cols matrix) (invNative matrix)

-- Wrap a successful solve with dimensions determined by the coefficient and
-- right-hand-side shapes.
linearSolve :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
linearSolve coefficients rhs = fmap
    (overrideMatrixDims (cols coefficients) (cols rhs))
    (linearSolveNative coefficients rhs)

linearSolveLS :: Matrix Double -> Matrix Double -> Matrix Double
linearSolveLS coefficients rhs = overrideMatrixDims (cols coefficients) (cols rhs)
    (linearSolveLSNative coefficients rhs)

chol :: Matrix Double -> Matrix Double
chol matrix = overrideMatrixDims (rows matrix) (cols matrix) (cholNative matrix)

expm :: Matrix Double -> Matrix Double
expm matrix = overrideMatrixDims (rows matrix) (cols matrix) (expmNative matrix)

foreign import trcall "Matrix:eigSHNative" eigSHNative :: Matrix Double -> (Vector Double, Matrix Double)
foreign import trcall "Matrix:eigenvaluesSHNative" eigenvaluesSHNative :: Matrix Double -> Vector Double
foreign import trcall "Matrix:svdNative" svdNative :: Int -> Matrix Double -> (Matrix Double, (Vector Double, Matrix Double))
foreign import trcall "Matrix:singularValuesNative" singularValuesNative :: Matrix Double -> Vector Double
foreign import trcall "Matrix:qrNative" qrNative :: Int -> Matrix Double -> (Matrix Double, Matrix Double)

-- Wrap a symmetric eigendecomposition without reading native result sizes.
eigSH :: Matrix Double -> (Vector Double, Matrix Double)
eigSH matrix = (overrideVectorSize dimension values,
                overrideMatrixDims dimension dimension vectors)
  where
    dimension = rows matrix
    (values, vectors) = eigSHNative matrix

eigenvaluesSH :: Matrix Double -> Vector Double
eigenvaluesSH matrix = overrideVectorSize (rows matrix)
    (eigenvaluesSHNative matrix)

-- Wrap one native SVD computation with dimensions derived from its input.
svdResult :: Int -> Matrix Double
          -> (Matrix Double, Vector Double, Matrix Double)
svdResult thin matrix = (overrideMatrixDims rowCount uColumns u,
                         overrideVectorSize k singular,
                         overrideMatrixDims columnCount vColumns v)
  where
    rowCount = rows matrix
    columnCount = cols matrix
    k = min rowCount columnCount
    uColumns = if thin == 0 then rowCount else k
    vColumns = if thin == 0 then columnCount else k
    (u, (singular, v)) = svdNative thin matrix

svd :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
svd = svdResult 0

thinSVD :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
thinSVD = svdResult 1

singularValues :: Matrix Double -> Vector Double
singularValues matrix = overrideVectorSize (min (rows matrix) (cols matrix))
    (singularValuesNative matrix)

qr :: Matrix Double -> (Matrix Double, Matrix Double)
qr = qrResult 0

thinQR :: Matrix Double -> (Matrix Double, Matrix Double)
thinQR = qrResult 1

-- Wrap one native QR computation with full or thin dimensions from its input.
qrResult :: Int -> Matrix Double -> (Matrix Double, Matrix Double)
qrResult thin matrix = (overrideMatrixDims rowCount k q,
                        overrideMatrixDims k columnCount r)
  where
    rowCount = rows matrix
    columnCount = cols matrix
    k = if thin == 0 then rowCount else min rowCount columnCount
    (q, r) = qrNative thin matrix
