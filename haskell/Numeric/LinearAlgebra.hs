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
    , tr
    , scale
    , dot
    , (<.>)
    , (#>)
    , (<#)
    , (<>)
    , outer
    , optimiseMult
    ) where

import Prelude hiding ((<>))
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
