module Probability.Distribution.Dirichlet
    (module Probability.Distribution.Dirichlet,
     module Probability.Distribution.Dirichlet.Density) where

import Probability.Random
import Probability.Distribution.Gamma
import Probability.Distribution.List
import MCMC.Moves.Real
import Probability.Distribution.Dirichlet.Density
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Sample gamma variables at the family rate, but update their redundant common scale at rate 1.
-- Common scaling preserves normalized values; the group move supplies the Jacobian.
sampleDirichlet as = do
  let gammaRate = 1 / sqrt (fromIntegral $ length as)
  vs <- (RanSamplingRate gammaRate $ mapM (\a -> sample $ gamma a 1) as)
          `withTKEffect` (\vs -> addMove 1 $ scaleGroupSlice vs)
  return $ map (/sum vs) vs

newtype Dirichlet = Dirichlet [Double]

instance Dist Dirichlet where
    type Result Dirichlet = [Double]
    distName _ = "dirichlet"

instance IOSampleable Dirichlet where
    sampleIO (Dirichlet as) = sampleIO $ sampleDirichlet as

instance HasPdf Dirichlet where
    pdf (Dirichlet as) ps = dirichletDensity as ps

instance HasAnnotatedPdf Dirichlet

instance Sampleable Dirichlet where
    sample (Dirichlet as) = sampleDirichlet as


dirichlet as = Dirichlet as


data SymmetricDirichlet = SymmetricDirichlet Int Double

instance Dist SymmetricDirichlet where
    type Result SymmetricDirichlet = [Double]
    distName _ = "symmetric_dirichlet"

instance HasPdf SymmetricDirichlet where
    pdf (SymmetricDirichlet n a) ps
      | length ps /= n = 0
      | otherwise = dirichletDensity (replicate n a) ps

instance HasAnnotatedPdf SymmetricDirichlet

instance Sampleable SymmetricDirichlet where
    -- Keep IID so changes in n reuse Gamma variables; the scale move remains on raw weights.
    sample (SymmetricDirichlet n a) = do
      ws <- (sample $ iid n (gamma a 1)) `withTKEffect` (\ws -> addMove 1 $ scaleGroupSlice ws)
      return $ map (/sum ws) ws

instance IOSampleable SymmetricDirichlet where
    sampleIO dist = sampleIO (sample dist)

symmetricDirichlet = SymmetricDirichlet


-----

newtype DirichletOn a = DirichletOn (Map.Map a Double)

instance Dist (DirichletOn a) where
    type Result (DirichletOn a) = Map.Map a Double
    distName _ = "dirichlet_on"

instance IOSampleable (DirichletOn a) where
    sampleIO (DirichletOn concentrations) = do
      ps <- sampleIO $ dirichlet $ Map.elems concentrations
      return $ Map.fromDistinctAscList $ zip (Map.keys concentrations) ps

instance Eq a => HasPdf (DirichletOn a) where
    -- Ascending keys give a linear domain check before pairing concentration and probability values.
    pdf (DirichletOn concentrations) probabilities
      | Map.size concentrations /= Map.size probabilities = 0
      | Map.keys concentrations /= Map.keys probabilities = 0
      | otherwise = pdf (Dirichlet $ Map.elems concentrations) (Map.elems probabilities)


instance Eq a => HasAnnotatedPdf (DirichletOn a)

instance Sampleable (DirichletOn a) where
    sample (DirichletOn concentrations) = do
      ps <- sample $ dirichlet $ Map.elems concentrations
      return $ Map.fromDistinctAscList $ zip (Map.keys concentrations) ps

dirichletOn = DirichletOn

-- Build the concentration map in linear time from the Set's distinct ascending keys.
symmetricDirichletOn items a =
  dirichletOn $ Map.fromDistinctAscList [(item, a) | item <- Set.toAscList items]
