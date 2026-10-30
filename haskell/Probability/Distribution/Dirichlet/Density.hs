-- Shared by the vector Dirichlet and the component-list Dirichlet mixture.
module Probability.Distribution.Dirichlet.Density where

import Numeric.Log (Log)
import Numeric.LinearAlgebra (Vector, fromList)

foreign import trcall "Distribution:dirichlet_density" dirichletDensityNative :: Vector Double -> Vector Double -> Log Double

-- Reuse the native Dirichlet kernel without making List and Dirichlet import each other.
dirichletDensity as ps = dirichletDensityNative (fromList as) (fromList ps)
