module SModel (module SModel,
               module SModel.Nucleotides,
               module SModel.Doublets,
               module SModel.Codons,
               module SModel.ReversibleMarkov,
               module SModel.NonReversibleMarkov,
               module SModel.Parsimony,
               module SModel.Simple,
               module SModel.Rate,
               module SModel.MixtureModel,
               module SModel.MixtureModels,
               module SModel.Empirical,
               module SModel.MarkovModulated,
               module SModel.MutSel,
               module SModel.MultiFrequency,
               module SModel.BranchModel,
               module SModel.RNAEdit,
               module SModel.BranchSiteMixture,
               frequencies_from_dict) where

import Probability.Distribution.Discrete
import Probability.Distribution.Beta
import Probability.Distribution.Gamma
import Probability.Distribution.Transform
import Bio.Alphabet
import Bio.Sequence
import Tree
import Parameters
import Data.Array
    
import SModel.Nucleotides
import SModel.Doublets
import SModel.Codons
import SModel.ReversibleMarkov
import SModel.NonReversibleMarkov
import SModel.Parsimony
import SModel.Simple
import SModel.Rate
import SModel.MixtureModel
import SModel.MixtureModels
import SModel.Empirical
import SModel.MarkovModulated
import SModel.MultiFrequency
import SModel.MutSel
import SModel.BranchModel
import SModel.RNAEdit
import SModel.BranchSiteMixture

import Data.Matrix

data F81 = F81 Alphabet (EVector Int) () (EVector Double)

-- We need to combine branch lengths and rate matrices to get transition probability matrices.
-- We need to combine mixtures of rate matrices.
-- We need to combine mixtures of transition probability matrices.
-- Should we combine mixture only at one of the levels?
-- Should we select branch-specific models at the level of rate matrices, or the level of transition probability matrices, or both?


infixl 2 +>
submodel +> model = model submodel

--
m1aOmegaDist f1 w1 = Discrete [(w1, f1), (1, 1-f1)]

m2aOmegaDist f1 w1 posP posW = addComponent (m1aOmegaDist f1 w1) (posW, posP)

m2aTestOmegaDist f1 w1 posP posW 0 = m2aOmegaDist f1 w1 posP 1
m2aTestOmegaDist f1 w1 posP posW _ = m2aOmegaDist f1 w1 posP posW

m3OmegaDist ps omegas = Discrete $ zip' omegas ps

m3pOmegaDist ps omegas posP posW = addComponent (m3OmegaDist ps omegas) (posW, posP)

m3TestOmegaDist ps omegas posP posW 0 = m3pOmegaDist ps omegas posP 1
m3TestOmegaDist ps omegas posP posW _ = m3pOmegaDist ps omegas posP posW

-- The M7 is just a beta distribution
-- gamma' = var(x)/(mu*(1-mu)) = 1/(a+b+1) = 1/(n+1)
m7OmegaDist mu gamma nBins = uniformDiscretize (beta a b) nBins where cap = min (mu/(1+mu)) ((1-mu)/(2-mu))
                                                                      gamma' = gamma*cap
                                                                      n = (1/gamma')-1
                                                                      a = n*mu
                                                                      b = n*(1 - mu)

-- The M8 is a beta distribution, where a fraction posP of sites have omega posW
m8OmegaDist mu gamma nBins posP posW = addComponent (m7OmegaDist mu gamma nBins) (posW, posP)

m8aOmegaDist mu gamma nBins posP = m8OmegaDist mu gamma nBins posP 1

m8aTestOmegaDist mu gamma nBins posP posW 0 = m8OmegaDist mu gamma nBins posP 1
m8aTestOmegaDist mu gamma nBins posP posW _ = m8OmegaDist mu gamma nBins posP posW

--  w1 <- uniform 0 1
--  [f1, f2] <- symmetricDirichlet 2 1
m1a w1 f1 modelFunc = modelFunc <$> m1aOmegaDist f1 w1

m2a w1 f1 posP posW modelFunc = modelFunc <$> m2aOmegaDist f1 w1 posP posW

m2aTest w1 f1 posP posW posSelection modelFunc = modelFunc <$> m2aTestOmegaDist f1 w1 posP posW posSelection

m3 omegaDist modelFunc = modelFunc <$> omegaDist

m3Test ps omegas posP posW posSelection modelFunc = modelFunc <$> m3TestOmegaDist ps omegas posP posW posSelection

m7 mu gamma nBins modelFunc = modelFunc <$> m7OmegaDist mu gamma nBins

m8 mu gamma nBins posP posW modelFunc = modelFunc <$> m8OmegaDist mu gamma nBins posP posW

m8a mu gamma nBins posP modelFunc = modelFunc <$> m8aOmegaDist mu gamma nBins posP

m8aTest mu gamma nBins posP posW posSelection modelFunc = modelFunc <$> m8aTestOmegaDist mu gamma nBins posP posW posSelection

-- OK, so if I change this from [Mixture Omega] to Mixture [Omega] or Mixture (\Int -> Omega), how do I apply the function modelFunc to all the omegas?
branchSite fs ws posP posW branchCats modelFunc = MixtureModels branchCats [bgMixture,fgMixture]
-- background omega distribution -- where the last omega is 1 (neutral)
    where bgDist = Discrete $ zip (ws ++ [1]) fs
-- accelerated omega distribution -- posW for all categories
          accelDist = Discrete $ zip (repeat posW) fs
-- background branches always use the background omega distribution              
          bgMixture = modelFunc <$> mix [1-posP, posP] [bgDist, bgDist]
-- foreground branches use the foreground omega distribution with probability posP
          fgMixture = modelFunc <$> mix [1-posP, posP] [bgDist, accelDist]

branchSiteTest fs ws posP posW posSelection branchCats modelFunc = branchSite fs ws posP posW' branchCats modelFunc
    where posW' = if (posSelection == 1) then posW else 1

gammaRatesDist alpha = gamma alpha (1/alpha)

gammaRates alpha n base = rateMixtureUnifBins base (gammaRatesDist alpha) n

logNormalRatesDist sigmaOverMu = logNormal lmu lsigma where x = log(1+sigmaOverMu^2)
                                                            lmu = -0.5*x
                                                            lsigma = sqrt x

logNormalRates sigmaOverMu n base = rateMixtureUnifBins base (logNormalRatesDist sigmaOverMu) n

-- join collapses a Discrete (Discrete a) -> Discrete a
freeRates rateDist base = join $ (\r -> scale r base) <$> rateDist

-- * OK... so a mixture of rate matrices is NOT the same as a mixture of exponentiated matrices, because the rate matrices are scaled relative to each other.
--   ** Hmm... THAT might explain why the mixtures aren't working well!  We need to scale each of THOSE components separately.

-- * In theory, we should allow each mixture component to have a different number of states.  This would require
--   that we either split the condition likelihoods into per-component objects, or reserve sum(i,smap(i)) spots per cell.
--   Probably the latter one would be fine.

-- * The model from Sergei Kosakovsky-Pond is a SModelOnTreeMixture, since it is a mixture at the matrix level.
-- * The MBR models are also SModelOnTree Mixtures, since they are also mixtures at the matrix level.
--   + We should be able to get them by combining SingleBranchLengthModels.

-- OK... so a mixture of rate matrices is NOT the same as a mixture of exponentiated matrices, because the rate matrices are scale with respect to each other.
-- So, we can have
--   ReversibleMarkov                          -- rate matrix
--   MixtureModel ReversibleMarkov             -- mixture of rate matrices
--   MixtureModels branchCats MixtureModel     -- per-branch mixture of rate matrices, where component i always has the same frequencies.

-- We can construct mixtures of these things with e.g. gamma rate models.
--   Gamma rate models SHOULD be able to construct unit_mixtures WITHOUT the use of mmm or unitMixture now.
--   We should also be able to constructing mixtures of mixtures of rate matrices -> mixtures of rate matrices.  This sounds like the join operation.

-- class SModelOnTree a where
--   branch_transition_p       :: (SingleBranchLengthModel a) Int -> EVector (Matrix Double)
--   distribution              :: a -> [Double]
--   weighted_frequency_matrix :: a -> Matrix Double
--   weighted_matrix           :: a -> Matrix Double
--   nBaseModels               :: a -> Int
--   stateLetters              :: a -> EVector
--   getAlphabet               :: a -> b
--   componentFrequencies      :: a -> Int -> EVector

-- How about
--   scale :: a -> a ?
--   qExp  :: a -> Matrix Double?
-- What kind of things can be scaled?  Things built on rate matrices, I guess?

-- If a mixture of mixture can be flattened, Mixture (Mixture) -> Mixture, isn't that like the monadic operation "join"?

-- Instances:
--   * ReversibleMarkovModel
--   * MixtureModel
--   * MixtureModels
--   * SModelOnTree a => SModelOnTreeMixture [(Double,a)]

-- So, the question is, can we avoid distribution on things like mixtures of Rates.

-- So, how are we going to handle rate scaling?  That should be part of the model!
