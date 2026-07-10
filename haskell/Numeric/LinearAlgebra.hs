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
    , (%*%)
    ) where

import Numeric.LinearAlgebra.Data
import Numeric.Matrix ()
import Numeric.Vector ()
