module Numeric.LinearAlgebra
    ( R
    , I
    , Vector
    , Matrix
    , Element(..)
    , IndexOf
    , Container(..)
    , vector
    , range
    , idxs
    , toList
    , rows
    , cols
    , fromLists
    , toLists
    , flatten
    , reshape
    , asRow
    , asColumn
    , ident
    , tr
    , scale
    , dot
    , (<.>)
    , (#>)
    , (<#)
    , (<>)
    , outer
    ) where

import Prelude hiding ((<>))
import Numeric.LinearAlgebra.Data
import Numeric.Matrix ()
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
