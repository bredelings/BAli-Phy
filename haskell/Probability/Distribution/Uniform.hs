module Probability.Distribution.Uniform where

import Probability.Random
import Control.Monad.IO.Class
import MCMC

foreign import bpcall "Distribution:" uniform_density :: Double -> Double -> Double -> LogDouble
foreign import bpcall "Distribution:sample_uniform" builtin_sample_uniform :: Double -> Double -> Int -> Double

uniform_bounds l u = between l u
uniform_effect l u x = add_move $ slice_sample_real_random_variable x (uniform_bounds l u)
sample_uniform l u = makeIO $ builtin_sample_uniform l u
ran_sample_uniform l u = RanAtomic (uniform_effect l u) (sample_uniform l u)

uniform_quantile l u x | x < l      = 0
                       | x > u      = 1
                       | otherwise  = (x-l)/(u-l)



uniform_int_quantile l u x | x <= l     = 0
                           | x > u      = 1
                           | otherwise  = fromIntegral (x-l) / fromIntegral (u-l+1)

foreign import bpcall "Distribution:uniform_int_density" uniform_int_density :: Int -> Int -> Int -> LogDouble
foreign import bpcall "Distribution:sample_uniform_int" builtin_sample_uniform_int :: Int -> Int -> Int -> Int

uniform_int_bounds l u = integer_between l u
uniform_int_effect l u x = add_move $ slice_sample_integer_random_variable x (uniform_int_bounds l u)
sample_uniform_int l u = makeIO $ builtin_sample_uniform_int l u
ran_sample_uniform_int l u = RanAtomic (uniform_int_effect l u) (sample_uniform_int l u)

class HasUniform d where
    uniform :: Double -> Double -> d Double
    uniform_int :: Int -> Int -> d Int

instance HasUniform Distribution where
    uniform l u = Distribution "uniform" (make_densities $ uniform_density l u) (uniform_quantile l u) (ran_sample_uniform l u) (uniform_bounds l u)
    uniform_int l u = Distribution "uniform_continuous" (make_densities $ uniform_int_density l u) (uniform_int_quantile l u) (ran_sample_uniform_int l u) (integer_between l u)

instance HasUniform Random where
    uniform l u = RanDistribution $ uniform l u
    uniform_int l u = RanDistribution $ uniform_int l u
