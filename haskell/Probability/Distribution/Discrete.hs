module Probability.Distribution.Discrete where

import Probability.Random
import Data.List -- for foldl'

uniformQuantiles q n = map (\i -> q ((2*(intToDouble i)+1)/(intToDouble n)) ) (take n [1..])

mix fs ds = [(p*f, x) | (f, d) <- zip' fs ds, (p, x) <- d]

certainly x = [(1, x)]

extendDiscreteDistribution d p x = mix [p, 1-p] [certainly x, d]

average l = foldl' (\x y->(x+(fst y)*(snd y))) 0 l

uniformGrid n = [( 1/n', (2*i'+1)/(2*n') ) | i <- take n [0..], let n' = intToDouble n, let i'=intToDouble i]

uniformDiscretize dist n = [(p, quantile dist x) | (p,x) <- uniformGrid n]

