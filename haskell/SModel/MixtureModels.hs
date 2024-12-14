module SModel.MixtureModels where

import Bio.Alphabet
import SModel.Simple
import SModel.Rate
import SModel.MixtureModel
import Tree
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Markov (CTMC)

-- Currently we are weirdly duplicating the mixture probabilities for each component.
-- Probably the actual data-type is something like [(Double,\Int->a)] or [(Double,[a])] where all the [a] should have the same length.
-- This would be a branch-dependent mixture
data MixtureModels m = MixtureModels (IntMap Int) [Discrete m]

branch_categories (MixtureModels categories _) = categories

mmm branch_cats m = MixtureModels branch_cats [m]

instance HasAlphabet m => HasAlphabet (MixtureModels m) where
    getAlphabet               (MixtureModels _ (m:ms)) = getAlphabet m

instance (HasSMap m, CTMC m, HasAlphabet m, RateModel m, HasBranchLengths t, SimpleSModel t m) => SimpleSModel t (MixtureModels m) where
    type instance IsReversible (MixtureModels m) = IsReversible m
    branch_transition_p (SModelOnTree tree smodel@(MixtureModels branchCats mms) f) b = branch_transition_p (SModelOnTree tree mx f) b
        where mx = mms!!(branchCats IntMap.! undirectedName b)
    distribution           (SModelOnTree tree (MixtureModels _ (m:ms)) f) = distribution (SModelOnTree tree m f)
    nBaseModels            (SModelOnTree tree (MixtureModels _ (m:ms)) f) = nBaseModels (SModelOnTree tree m f)
    stateLetters           (SModelOnTree tree (MixtureModels _ (m:ms)) f) = stateLetters (SModelOnTree tree m f)
    componentFrequencies   (SModelOnTree tree (MixtureModels _ (m:ms)) f) = componentFrequencies (SModelOnTree tree m f)


-- No Attribute
getForeground Nothing = 0
-- Attribute with no value
getForeground (Just Nothing) = 1
-- Attribute with value
getForeground (Just (Just text)) = read (T.unpack text) :: Int

foregroundBranches tree key = edgeAttributes tree (T.pack key) getForeground

-- OK, so if I change this from [Mixture Omega] to Mixture [Omega] or Mixture (\Int -> Omega), how do I apply the function modelFunc to all the omegas?
branchSite fs ws posP posW branchCats modelFunc = MixtureModels branchCats [bgMixture,fgMixture]
-- background omega distribution -- where the last omega is 1 (neutral)
    where bgDist = mkDiscrete (ws ++ [1]) fs
-- accelerated omega distribution -- posW for all categories
          accelDist = mkDiscrete (repeat posW) fs
-- background branches always use the background omega distribution
          bgMixture = modelFunc <$> mix [1-posP, posP] [bgDist, bgDist]
-- foreground branches use the foreground omega distribution with probability posP
          fgMixture = modelFunc <$> mix [1-posP, posP] [bgDist, accelDist]

branchSiteTest fs ws posP posW posSelection branchCats modelFunc = branchSite fs ws posP posW' branchCats modelFunc
    where posW' = if (posSelection == 1) then posW else 1

