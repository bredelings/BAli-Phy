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
    ) where

import Foreign.Maybe (CMaybe, fromCMaybe)
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
