module Probability.Distribution.Poisson where

import Probability.Random
import MCMC
import Probability.Distribution.List
import Probability.Distribution.Uniform

foreign import bpcall "Distribution:" poisson_density :: Double -> Int -> LogDouble
foreign import bpcall "Distribution:" sample_poisson :: Double -> IO Int

data Poisson = Poisson Double

instance Dist Poisson where
    type Result Poisson = Int
    dist_name _ = "poisson"

instance IOSampleable Poisson where
    sampleIO (Poisson mu) = sample_poisson mu

instance HasPdf Poisson where
    pdf (Poisson mu) n = poisson_density mu n

instance Dist1D Poisson where
    cdf (Poisson mu) = undefined

instance MaybeMean Poisson where
    maybeMean (Poisson mu) = Just mu

instance Mean Poisson

instance MaybeVariance Poisson where
    maybeVariance (Poisson mu)= Just mu

instance Variance Poisson

instance HasAnnotatedPdf Poisson where
    annotated_densities dist@(Poisson mu) n = do
        in_edge "mu" mu
        return [pdf dist n]

instance Sampleable Poisson where
    sample dist = RanDistribution2 dist poisson_effect
    
poisson_bounds = integer_above 0

poisson_effect x = do
   add_move $ slice_sample_integer_random_variable x poisson_bounds
   add_move $ inc_dec_mh x poisson_bounds

poisson mu = Poisson mu

------------------------------------------------------------
data PoissonProcess = PoissonProcess Double Double Double

instance Dist PoissonProcess where
    type Result PoissonProcess = [Double]
    dist_name _ = "poisson_process"

instance IOSampleable PoissonProcess where
    sampleIO (PoissonProcess rate t1 t2) = do
       n <- sampleIO $ poisson (rate * (t2-t1))
       xs <- sampleIO $ iid n (uniform t1 t2)
       return $ sort xs

instance HasPdf PoissonProcess where
    pdf (PoissonProcess rate t1 t2) = poisson_process_density rate t1 t2

instance Sampleable PoissonProcess where
    sample (PoissonProcess rate t1 t2) = do
       n <- sample $ poisson (rate * (t2-t1))
       xs <- sample $ iid n (uniform t1 t2)
       return $ sort xs

instance HasAnnotatedPdf PoissonProcess where
    annotated_densities (PoissonProcess rate t1 t2) = make_densities $ poisson_process_density rate t1 t2

--- Poisson process, constant rate --
poisson_process_density' rate t1 t2 n = expTo $ (-rate*(t2-t1)) + (fromIntegral n * log rate)
poisson_process_density  rate t1 t2 points = poisson_process_density' rate t1 t2 n where n = length points
sample_poisson_process rate t1 t2 = do
  n <- sample $ poisson (rate * (t2-t1))
  xs <- sample $ iid n (uniform t1 t2)
  return $ sort xs

--- Poisson process, piecewise constant

poisson_process rate t1 t2 = PoissonProcess rate t1 t2

------------------------------------------------------------
data PoissonProcesses = PoissonProcesses [(Double,Double,Double)]

instance Dist PoissonProcesses where
    type Result PoissonProcesses = [Double]
    dist_name _ = "poisson_processes"

instance HasPdf PoissonProcesses where
    pdf dist = density dist

instance HasAnnotatedPdf PoissonProcesses where
    annotated_densities (PoissonProcesses intervals) = make_densities' $ poisson_processes_densities intervals

instance Sampleable PoissonProcesses where
    sample (PoissonProcesses intervals) = sample_poisson_processes intervals

dropCount p xs = go p 0 xs where
    go p n [] = (n,[])
    go p n xs@(x:xs') | p x        = go p (n+1) xs'
                      | otherwise  = (n,xs)

poisson_processes_densities [] [] = []
poisson_processes_densities [] _  = [0]
poisson_processes_densities ((rate,t1,t2):intervals) points = poisson_process_density' rate t1 t2 n:poisson_processes_densities intervals remaining_points
    where (n,remaining_points) = dropCount (<= t2) points

sample_poisson_processes [] = return []
sample_poisson_processes ((rate,t1,t2):intervals) = do
  points1 <- sample_poisson_process rate t1 t2
  points2 <- sample_poisson_processes intervals
  -- FIXME - this doesn't seem very efficient!
  return $ points1 ++ points2


poisson_processes intervals = PoissonProcesses intervals

