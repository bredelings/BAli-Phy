module Probability.Distribution.Dirichlet where

import Probability.Random
import Probability.Distribution.Gamma
import Probability.Distribution.List
import MCMC.Moves.Real

foreign import bpcall "Distribution:dirichlet_density" builtinDirichletDensity :: EVector Double -> EVector Double -> LogDouble
dirichletDensity as ps = builtinDirichletDensity (toVector as) (toVector ps)

-- The `dirichlet` does not handle cases where the number of as changes in a graceful way: all entries are resampled!
sampleDirichlet as = do vs <- mapM (\a-> sample $ gamma a 1) as
                        return $ map (/(sum vs)) vs

data Dirichlet = Dirichlet [Double]

instance Dist Dirichlet where
    type Result Dirichlet = [Double]
    dist_name _ = "dirichlet"

instance IOSampleable Dirichlet where
    sampleIO (Dirichlet as) = sampleIO $ sampleDirichlet as

instance HasPdf Dirichlet where
    pdf (Dirichlet as) ps = dirichletDensity as ps

instance HasAnnotatedPdf Dirichlet where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable Dirichlet where
    sample dist@(Dirichlet as) = RanSamplingRate (1/sqrt(fromIntegral $ length as)) $ sampleDirichlet as


dirichlet as = Dirichlet as


-- Is there a more graceful way to add a move here?
symmetricDirichlet n a = do
  ws <- (sample $ iid n (gamma a 1)) `withTKEffect` (\ws -> addMove 1 $ scaleGroupSlice ws)
  return $ map (/sum ws) ws


-----

data DirichletOn a = DirichletOn [a] [Double]

instance Dist (DirichletOn a) where
    type Result (DirichletOn a) = [(a,Double)]
    dist_name _ = "dirichlet_on"

instance IOSampleable (DirichletOn a) where
    sampleIO (DirichletOn items as) = do ps <- sampleIO $ dirichlet as
                                         return $ zip items ps

instance HasPdf (DirichletOn a) where
    pdf (DirichletOn _ as) itemPrs = pdf (Dirichlet as) ps where ps = map snd itemPrs


instance HasAnnotatedPdf (DirichletOn a) where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable (DirichletOn a) where
    sample (DirichletOn items as) = do ps <- sample $ dirichlet as
                                       return $ zip items ps

dirichletOn items ps = DirichletOn items ps

symmetricDirichletOn items a = dirichletOn items (replicate (length items) a)

