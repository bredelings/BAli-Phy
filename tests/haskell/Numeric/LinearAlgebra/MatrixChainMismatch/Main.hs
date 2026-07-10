{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import Data.Monoid (mconcat)
import Numeric.LinearAlgebra
import System.IO (print)

-- Ensure scalar removal does not conceal an incompatible non-scalar chain.
main = print $ mconcat [ (2 >< 3) [1..6] :: Matrix Int
                       , 2 :: Matrix Int
                       , (2 >< 2) [1..4] :: Matrix Int
                       ]
