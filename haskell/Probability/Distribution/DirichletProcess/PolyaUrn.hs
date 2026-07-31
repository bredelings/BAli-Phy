module Probability.Distribution.DirichletProcess.PolyaUrn
    ( dirichletProcess
    , dirichletProcessMixture
    ) where

import Probability.Random
import Probability.Distribution.Bernoulli
import Probability.Distribution.List
import Probability.Distribution.Uniform
import MCMC.Moves.Integer (gibbsSampleCategoricalBounded)
import qualified Data.Vector as V

-- Observation i starts a new cluster by parenting itself with mass alpha/(alpha+i); each
-- earlier observation has mass 1/(alpha+i).  A size-m cluster therefore receives mass
-- m/(alpha+i), exactly the Polya-urn predictive rule, and resolving parents finds its atom.
data PolyaUrnParent = PolyaUrnParent Double Int

-- Dividing alpha/(alpha+i) by alpha gives 1/(1+i/alpha), whose limits are
-- also the required probabilities zero at alpha=0 and one at alpha=infinity.
freshParentProbability :: Double -> Int -> Double
freshParentProbability alpha index = 1 / (1 + fromIntegral index / alpha)

instance Dist PolyaUrnParent where
    type Result PolyaUrnParent = Int
    distName _ = "dirichlet process parent"

instance IOSampleable PolyaUrnParent where
    -- Draw the fresh/reused decision first, then choose an earlier parent uniformly in O(1) work.
    -- These sampleIO calls are atomic initialization, so they install no Bernoulli or uniform kernels.
    sampleIO (PolyaUrnParent alpha index)
      | index == 0 = return 0
      | otherwise = do
          fresh <- sampleIO $ bernoulli (freshParentProbability alpha index)
          if fresh == 1
            then return index
            else sampleIO $ uniform_int 0 (index - 1)

instance HasPdf PolyaUrnParent where
    -- A fresh parent has mass alpha/(alpha+i); each of the i earlier parents has mass 1/(alpha+i).
    pdf (PolyaUrnParent alpha index) parent
      | index == 0 && parent == 0 = 1
      | index > 0 && parent == index = doubleToLogDouble $ freshParentProbability alpha index
      | index > 0 && parent >= 0 && parent < index = doubleToLogDouble $ 1 / denominator
      | otherwise = 0
      where denominator = alpha + fromIntegral index

instance HasAnnotatedPdf PolyaUrnParent where
    -- Register alpha as an input so changing concentration invalidates parent probabilities, not structure.
    annotatedDensities dist@(PolyaUrnParent alpha _) parent = do
      in_edge "alpha" alpha
      return ([pdf dist parent], ())

instance Sampleable PolyaUrnParent where
    sample dist@(PolyaUrnParent _ index)
      | index == 0 = RanDistribution2 dist doNothing
      | otherwise = RanDistribution2 dist (parentEffect index)

-- Every nondeterministic parent uses the bounded categorical Gibbs kernel.
parentEffect index parent = addMove 1 $ gibbsSampleCategoricalBounded parent (index + 1)

-- Resolve each observation by following parents to its atom. Every non-self parent is smaller than
-- its child index, so recursion strictly decreases and must terminate at a self-parented atom.
resolveParentValues :: V.Vector Int -> V.Vector a -> V.Vector a
resolveParentValues parents atoms = values
  where
    values = V.generate (V.length parents) resolve
    -- Retain the resolved vector recursively so repeated references share the same atom computation.
    resolve index
      | parent == index = atoms V.! index
      | otherwise = values V.! parent
      where parent = parents V.! index

-- Generate ordered Pólya-urn parents and resolve them against interchangeable IID atoms.
-- Rebuilding the boxed vectors when n changes is intentional; only the retained prefix can survive.
dirichletProcess :: Sampleable d => Int -> Double -> d -> Random [Result d]
dirichletProcess n alpha dist = lazy $ do
  -- iid already scales atom moves; scale only the separate O(n) parent family here.
  parents <- RanSamplingRate familyRate $
             sequence [sample $ PolyaUrnParent alpha index | index <- [0..n-1]]
  atoms <- prior $ iid n dist
  let values = resolveParentValues (V.fromList parents) (V.fromList atoms)
  return $ V.toList values
  where familyRate = 1 / sqrt (fromIntegral n)

-- Draw shared component distributions from a DP, then lazily draw one ordered observation from each.
-- The observation rate is separate because dirichletProcess already scales parents and components.
dirichletProcessMixture
  :: (Sampleable d, Sampleable (Result d))
  => Int -> Double -> d -> Random [Result (Result d)]
dirichletProcessMixture n alpha dist = lazy $ do
  components <- dirichletProcess n alpha dist
  RanSamplingRate familyRate $ sequence [sample component | component <- components]
  where familyRate = 1 / sqrt (fromIntegral n)
