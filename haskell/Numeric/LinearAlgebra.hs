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

foreign import bpcall "Matrix:detNative" detNative :: NativeMatrix Double -> Double
foreign import bpcall "Matrix:invNative" invNative :: NativeMatrix Double -> NativeMatrix Double
foreign import bpcall "Matrix:linearSolveNative" linearSolveNative :: NativeMatrix Double -> NativeMatrix Double -> CMaybe (NativeMatrix Double)
foreign import bpcall "Matrix:linearSolveLSNative" linearSolveLSNative :: NativeMatrix Double -> NativeMatrix Double -> NativeMatrix Double
foreign import bpcall "Matrix:cholNative" cholNative :: NativeMatrix Double -> NativeMatrix Double
foreign import bpcall "Matrix:expmNative" expmNative :: NativeMatrix Double -> NativeMatrix Double

det :: Matrix Double -> Double
det = detNative . nativeMatrix

inv :: Matrix Double -> Matrix Double
inv matrix = matrixFromNative (rows matrix) (cols matrix) (invNative (nativeMatrix matrix))

-- Wrap a successful solve with dimensions determined by the coefficient and
-- right-hand-side shapes.
linearSolve :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
linearSolve coefficients rhs =
    case fromCMaybe (linearSolveNative (nativeMatrix coefficients) (nativeMatrix rhs)) of
        Nothing -> Nothing
        Just payload -> Just (matrixFromNative (cols coefficients) (cols rhs) payload)

linearSolveLS :: Matrix Double -> Matrix Double -> Matrix Double
linearSolveLS coefficients rhs = matrixFromNative (cols coefficients) (cols rhs)
    (linearSolveLSNative (nativeMatrix coefficients) (nativeMatrix rhs))

chol :: Matrix Double -> Matrix Double
chol matrix = matrixFromNative (rows matrix) (cols matrix) (cholNative (nativeMatrix matrix))

expm :: Matrix Double -> Matrix Double
expm matrix = matrixFromNative (rows matrix) (cols matrix) (expmNative (nativeMatrix matrix))

foreign import bpcall "Matrix:eigSHNative" eigSHNative :: NativeMatrix Double -> EPair (NativeVector Double) (NativeMatrix Double)
foreign import bpcall "Matrix:eigenvaluesSHNative" eigenvaluesSHNative :: NativeMatrix Double -> NativeVector Double
foreign import bpcall "Matrix:svdNative" svdNative :: Int -> NativeMatrix Double -> EPair (NativeMatrix Double) (EPair (NativeVector Double) (NativeMatrix Double))
foreign import bpcall "Matrix:singularValuesNative" singularValuesNative :: NativeMatrix Double -> NativeVector Double
foreign import bpcall "Matrix:qrNative" qrNative :: Int -> NativeMatrix Double -> EPair (NativeMatrix Double) (NativeMatrix Double)

-- Wrap a symmetric eigendecomposition without reading native result sizes.
eigSH :: Matrix Double -> (Vector Double, Matrix Double)
eigSH matrix = (vectorFromNative dimension values,
                matrixFromNative dimension dimension vectors)
  where
    dimension = rows matrix
    (values, vectors) = pair_from_c (eigSHNative (nativeMatrix matrix))

eigenvaluesSH :: Matrix Double -> Vector Double
eigenvaluesSH matrix = vectorFromNative (rows matrix)
    (eigenvaluesSHNative (nativeMatrix matrix))

-- Wrap one native SVD computation with dimensions derived from its input.
svdResult :: Int -> Matrix Double
          -> (Matrix Double, Vector Double, Matrix Double)
svdResult thin matrix = (matrixFromNative rowCount uColumns u,
                         vectorFromNative k singular,
                         matrixFromNative columnCount vColumns v)
  where
    rowCount = rows matrix
    columnCount = cols matrix
    k = min rowCount columnCount
    uColumns = if thin == 0 then rowCount else k
    vColumns = if thin == 0 then columnCount else k
    (u, rest) = pair_from_c (svdNative thin (nativeMatrix matrix))
    (singular, v) = pair_from_c rest

svd :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
svd = svdResult 0

thinSVD :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
thinSVD = svdResult 1

singularValues :: Matrix Double -> Vector Double
singularValues matrix = vectorFromNative (min (rows matrix) (cols matrix))
    (singularValuesNative (nativeMatrix matrix))

qr :: Matrix Double -> (Matrix Double, Matrix Double)
qr = qrResult 0

thinQR :: Matrix Double -> (Matrix Double, Matrix Double)
thinQR = qrResult 1

-- Wrap one native QR computation with full or thin dimensions from its input.
qrResult :: Int -> Matrix Double -> (Matrix Double, Matrix Double)
qrResult thin matrix = (matrixFromNative rowCount k q,
                        matrixFromNative k columnCount r)
  where
    rowCount = rows matrix
    columnCount = cols matrix
    k = if thin == 0 then rowCount else min rowCount columnCount
    (q, r) = pair_from_c (qrNative thin (nativeMatrix matrix))
