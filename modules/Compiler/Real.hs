{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Real where

import Compiler.Base
import Compiler.Num
import Data.Ord      -- for <
import Data.Function -- for .

infixl 8 ^, ^^, **
infixl 7 /, `quot`, `rem`, `div`, `mod`

builtin / 2 "divide" "Prelude"
builtin div 2 "div" "Prelude"
builtin mod 2 "mod" "Prelude"
builtin quot 2 "quot" "Prelude"
builtin rem 2 "rem" "Prelude"

builtin log 1 "log" "Prelude"
builtin exp 1 "exp" "Prelude"
builtin sqrt 1 "sqrt" "Prelude"
builtin truncate 1 "truncate" "Prelude"
builtin ceiling 1 "ceiling" "Prelude"
builtin floor 1 "floor" "Prelude"
builtin round 1 "round" "Prelude"

builtin doubleToInt 1 "doubleToInt" "Prelude"
builtin ** 2 "pow" "Prelude"

builtin doubleToLogDouble 1 "doubleToLogDouble" "Prelude"
builtin intToDouble 1 "intToDouble" "Prelude"

-- We need == to use GHC's code directly
x0 ^ y0 | y0 < 0 = error("Negative exponent")
x ^ 1 = x
x ^ n = x*(x^(n-1))

recip y = 1.0/y -- should be 1/y

x ^^ n = if n >= 0 then x^n else recip (x^(negate n))
