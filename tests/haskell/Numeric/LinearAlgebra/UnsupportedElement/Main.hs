{-# LANGUAGE NoImplicitPrelude #-}

import Numeric.LinearAlgebra

bad :: Matrix Char
bad = (1 >< 1) ['x']
