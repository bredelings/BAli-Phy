module Probability.Distribution.Dirichlet where

import Probability.Random
import Probability.Distribution.Gamma
import Probability.Distribution.List
import MCMC.Moves.Real
import Numeric.LinearAlgebra (Vector, fromList)
import qualified Data.Map as Map

foreign import trcall "Distribution:dirichlet_density" dirichletDensityNative :: Vector Double -> Vector Double -> Log Double
dirichletDensity as ps = dirichletDensityNative
    (fromList as) (fromList ps)

-- The `dirichlet` does not handle cases where the number of as changes in a graceful way: all entries are resampled!
sampleDirichlet as = do vs <- mapM (\a-> sample $ gamma a 1) as
                        return $ map (/(sum vs)) vs

newtype Dirichlet = Dirichlet [Double]

instance Dist Dirichlet where
    type Result Dirichlet = [Double]
    distName _ = "dirichlet"

instance IOSampleable Dirichlet where
    sampleIO (Dirichlet as) = sampleIO $ sampleDirichlet as

instance HasPdf Dirichlet where
    pdf (Dirichlet as) ps = dirichletDensity as ps

instance HasAnnotatedPdf Dirichlet where
    annotatedDensities dist = make_densities $ pdf dist

instance Sampleable Dirichlet where
    sample dist@(Dirichlet as) = RanSamplingRate (1/sqrt(fromIntegral $ length as)) $ sampleDirichlet as


dirichlet as = Dirichlet as


-- Is there a more graceful way to add a move here?
symmetricDirichlet n a = do
  ws <- (sample $ iid n (gamma a 1)) `withTKEffect` (\ws -> addMove 1 $ scaleGroupSlice ws)
  return $ map (/sum ws) ws


-----

newtype DirichletOn a = DirichletOn (Map.Map a Double)

instance Dist (DirichletOn a) where
    type Result (DirichletOn a) = Map.Map a Double
    distName _ = "dirichlet_on"

instance IOSampleable (DirichletOn a) where
    sampleIO (DirichletOn concentrations) = do
      ps <- sampleIO $ dirichlet $ Map.elems concentrations
      return $ Map.fromDistinctAscList $ zip (Map.keys concentrations) ps

instance HasPdf (DirichletOn a) where
    pdf (DirichletOn concentrations) probabilities =
      pdf (Dirichlet $ Map.elems concentrations) (Map.elems probabilities)


instance HasAnnotatedPdf (DirichletOn a) where
    annotatedDensities dist = make_densities $ pdf dist

instance Sampleable (DirichletOn a) where
    sample (DirichletOn concentrations) = do
      ps <- sample $ dirichlet $ Map.elems concentrations
      return $ Map.fromDistinctAscList $ zip (Map.keys concentrations) ps

dirichletOn = DirichletOn

-- Convert a list domain to a concentration map, rejecting repeated category names.
symmetricDirichletOn items a
  | Map.size concentrations == length items = dirichletOn concentrations
  | otherwise = error "symmetricDirichletOn: repeated item"
  where concentrations = Map.fromList [(item, a) | item <- items]
