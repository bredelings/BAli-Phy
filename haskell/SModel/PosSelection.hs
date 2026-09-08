module SModel.PosSelection where

import SModel.ASRV
import SModel.BranchSiteMixture
import SModel.Property (emptyComponentAnnotations)
import Numeric.LinearAlgebra (Vector)
import Compiler.RealFloat (isInfinite)
import Reversible
import MCMC.Loggers (binaryIndicatorFields)
import MCMC.Types (ContextAction, Modifiable)
import Data.JSON (Object)
import qualified Data.Text as T

foreign import trcall "Distribution:betaQuadratureNative"
    betaQuadratureNative :: Double -> Double -> Int -> (Vector Double, Vector Double)

positiveSelectionFields :: Modifiable Int -> ContextAction Object
positiveSelectionFields = binaryIndicatorFields (T.pack "PosSelection")

m1aOmegaDist f1 w1 = Discrete [(w1, f1), (1, 1-f1)]

m2aOmegaDist f1 w1 posP posW = mix [1 - posP, posP] [m1aOmegaDist f1 w1, always posW]

m2aTestOmegaDist f1 w1 posP posW 0 = m2aOmegaDist f1 w1 posP 1
m2aTestOmegaDist f1 w1 posP posW _ = m2aOmegaDist f1 w1 posP posW

m3OmegaDist ps omegas = mkDiscrete omegas ps

-- M7 uses weighted beta quadrature. The beta variance identity
-- v = var(x)/(mu*(1-mu)) = 1/(a+b+1) gives concentration = a+b = 1/v-1.
m7OmegaDist mu v nBins
    | nBins <= 0 = error "m7OmegaDist: the number of nodes must be positive"
    | concentratedLimit = Discrete (replicate nBins (mu, 1 / fromIntegral nBins))
    | otherwise = quadratureDiscrete nBins (betaQuadratureNative a b nBins)
  where
    concentration = 1/v - 1
    a = concentration*mu
    b = concentration*(1-mu)
    -- Retain the known mean before reciprocal overflow loses the shape ratio. Vanishing
    -- variance gives a point at mu, unlike the two-endpoint limit of small concentration.
    concentratedLimit = mu > 0 && mu < 1 && v >= 0 && not (isInfinite v)
        && (v == 0 || (v > 0 && isInfinite (1/v)))

-- The M8 is a beta distribution, where a fraction posP of sites have omega posW
m8OmegaDist mu v nBins posP posW = mix [1 - posP, posP] [m7OmegaDist mu v nBins, always posW]

m8aOmegaDist mu v nBins posP = m8OmegaDist mu v nBins posP 1

m8aTestOmegaDist mu v nBins posP posW 0 = m8OmegaDist mu v nBins posP 1
m8aTestOmegaDist mu v nBins posP posW _ = m8OmegaDist mu v nBins posP posW

--  w1 <- uniform 0 1
--  [f1, f2] <- symmetricDirichlet 2 1
m1a w1 f1 modelFunc = modelFunc <$> m1aOmegaDist f1 w1

m2a w1 f1 posP posW modelFunc = modelFunc <$> m2aOmegaDist f1 w1 posP posW

m2aTest w1 f1 posP posW posSelection modelFunc = modelFunc <$> m2aTestOmegaDist f1 w1 posP posW posSelection

m3 omegaDist modelFunc = modelFunc <$> omegaDist

m3Test omegaDist posP posW posSelection modelFunc = modelFunc <$> mix [posP, 1-posP] [always posW', omegaDist]
    where posW' = case posSelection of 0 -> 1; 1 -> posW

m7 mu v nBins modelFunc = modelFunc <$> m7OmegaDist mu v nBins

m8 mu v nBins posP posW modelFunc = modelFunc <$> m8OmegaDist mu v nBins posP posW

m8a mu v nBins posP modelFunc = modelFunc <$> m8aOmegaDist mu v nBins posP

m8aTest mu v nBins posP posW posSelection modelFunc = modelFunc <$> m8aTestOmegaDist mu v nBins posP posW posSelection

-- Should we normalize the different entries to have the same rate?
busted omegaDist posP posW posSelection modelFunc =
    BranchSiteMixture (m3Test omegaDist posP posW posSelection modelFunc) SameEqs emptyComponentAnnotations

bustedS omegaDist posP posW posSelection alpha n modelFunc = gammaRatesOn alpha n $ always $ busted omegaDist posP posW posSelection modelFunc

-- * The model from Sergei Kosakovsky-Pond is a SModelOnTreeMixture, since it is a mixture at the matrix level.
-- * The MBR models are also SModelOnTree Mixtures, since they are also mixtures at the matrix level.
--   + We should be able to get them by combining SingleBranchLengthModels.
--
-- * OK... so a mixture of rate matrices is NOT the same as a mixture of exponentiated matrices, because the rate matrices are scaled relative to each other.
--   + Hmm... THAT might explain why the mixtures aren't working well!  We need to scale each of THOSE components separately.
--
-- * In theory, we should allow each mixture component to have a different number of states.  This would require
--   that we either split the condition likelihoods into per-component objects, or reserve sum(i,smap(i)) spots per cell.
--   Probably the latter one would be fine.
--
-- OK... so a mixture of rate matrices is NOT the same as a mixture of exponentiated matrices, because the rate matrices are scale with respect to each other.
-- So, we can have
--   ReversibleMarkov                              -- rate matrix
--   Discrete ReversibleMarkov                     -- site mixture of rate matrices
--   Discrete (BranchModel ReversibleMarkov)       -- site mixture of branch-dependent rate matrices
--
-- We can construct mixtures of these things with e.g. gamma rate models.
--   Gamma rate models can scale and flatten these mixtures directly.
--   We should also be able to constructing mixtures of mixtures of rate matrices -> mixtures of rate matrices.  This sounds like the join operation.

    
