{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Classes
import Compiler.Fractional
import Compiler.Num
import Compiler.RealFrac
import Data.Bool
import Data.Eq
import System.IO (print)

-- Check the specialized Double-to-Int operations for both signs and verify
-- that round resolves half-integers toward the nearest even integer.
main = print
    (truncate 1.9 == 1 && truncate (-1.9) == -1 &&
     ceiling 1.1 == 2 && ceiling (-1.1) == -1 &&
     floor 1.9 == 1 && floor (-1.1) == -2 && floor 0 == 0 &&
     round 0.5 == 0 && round 1.5 == 2 && round 2.5 == 2 &&
     round (-0.5) == 0 && round (-1.5) == -2 && round (-2.5) == -2)
