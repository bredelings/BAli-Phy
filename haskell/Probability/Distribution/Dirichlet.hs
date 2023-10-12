module Probability.Distribution.Dirichlet where

import Probability.Random
import Probability.Distribution.Gamma
import Probability.Distribution.List

foreign import bpcall "Distribution:dirichlet_density" builtin_dirichlet_density :: EVector Double -> EVector Double -> LogDouble
dirichlet_density as ps = builtin_dirichlet_density (list_to_vector as) (list_to_vector ps)

-- The `dirichlet` does not handle cases where the number of as changes in a ungraceful way: all entries are resampled!
sample_dirichlet as = do vs <- mapM (\a-> sample $ gamma a 1) as
                         return $ map (/(sum vs)) vs

data Dirichlet = Dirichlet [Double]

instance Dist Dirichlet where
    type Result Dirichlet = [Double]
    dist_name _ = "dirichlet"

instance IOSampleable Dirichlet where
    sampleIO (Dirichlet as) = sampleIO $ sample_dirichlet as

instance HasPdf Dirichlet where
    pdf (Dirichlet as) ps = dirichlet_density as ps

instance HasAnnotatedPdf Dirichlet where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable Dirichlet where
    sample dist@(Dirichlet as) = RanSamplingRate (1/sqrt(fromIntegral $ length as)) $ sample_dirichlet as


dirichlet as = Dirichlet as

symmetric_dirichlet n a = do
  ws <- sample $ iid n (gamma a 1)
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
    pdf (DirichletOn _ as) item_ps = pdf (Dirichlet as) ps where ps = map snd item_ps


instance HasAnnotatedPdf (DirichletOn a) where
    annotated_densities dist = make_densities $ pdf dist

instance Sampleable (DirichletOn a) where
    sample (DirichletOn items as) = do ps <- sample $ dirichlet as
                                       return $ zip items ps

dirichlet_on items ps = DirichletOn items ps

symmetric_dirichlet_on items a = dirichlet_on items (replicate (length items) a)

