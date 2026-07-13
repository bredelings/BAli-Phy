{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Fractional
import Compiler.Num
import Probability.Distribution.Multinomial (multinomial_density)
import System.IO (print)

-- Exercise primitive-vector probability boundaries without exposing their
-- internal unboxed representation in the public list-facing interface.
main = do
    print (multinomial_density 2 [0.25,0.75] [1,1])
    print (multinomial_density 2 [0.25,0.75] [2,1])
