module Probability.Distribution.Poisson where

import Probability.Random
import MCMC
import Probability.Distribution.List
import Probability.Distribution.Uniform

builtin poisson_density 2 "poisson_density" "Distribution"
builtin builtin_sample_poisson 2 "sample_poisson" "Distribution"

poisson_bounds = integer_above 0

poisson_effect x = do
   add_move $ slice_sample_integer_random_variable x poisson_bounds
   add_move $ inc_dec_mh x poisson_bounds

sample_poisson mu = RandomStructure poisson_effect modifiable_structure $ liftIO (IOAction (\s -> (s, builtin_sample_poisson mu s)))

poisson mu = Distribution (make_densities $ poisson_density mu) (no_quantile "Poisson") (sample_poisson mu) (integer_above 0)

--- Poisson process, constant rate --
poisson_process_density' rate t1 t2 n = expToLogDouble $ (-rate*(t2-t1)) + ((intToDouble n)*log rate)
poisson_process_density  rate t1 t2 points = poisson_process_density' rate t1 t2 n where n = length points
sample_poisson_process rate t1 t2 = do
  n <- poisson (rate * (t2-t1))
  xs <- iid n (uniform t1 t2)
  return $ sort xs

poisson_process rate t1 t2 = Distribution (make_densities $ poisson_process_density rate t1 t2) (no_quantile "poisson_process") (sample_poisson_process rate t1 t2) Nothing

--- Poisson process, piecewise constant

dropCount p xs = go p 0 xs where
    go p n [] = (n,[])
    go p n xs@(x:xs') | p x        = go p (n+1) xs'
                      | otherwise  = (n,xs)

poisson_processes_densities [] [] = []
poisson_processes_densities [] _  = [doubleToLogDouble 0.0]
poisson_processes_densities ((rate,t1,t2):intervals) points = poisson_process_density' rate t1 t2 n:poisson_processes_densities intervals remaining_points
    where (n,remaining_points) = dropCount (<= t2) points

sample_poisson_processes [] = return []
sample_poisson_processes ((rate,t1,t2):intervals) = do
  points1 <- sample_poisson_process rate t1 t2
  points2 <- sample_poisson_processes intervals
  -- FIXME - this doesn't seem very efficient!
  return $ points1 ++ points2

poisson_processes intervals = Distribution (poisson_processes_densities intervals) (no_quantile "poisson_processes") (sample_poisson_processes intervals) Nothing

