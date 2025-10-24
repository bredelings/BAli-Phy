module SModel.MixtureModels where

import Bio.Alphabet
import SModel.Simple
import Reversible
import SModel.Rate
import SModel.MixtureModel
import Tree
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Text as T

-- Currently we are weirdly duplicating the mixture probabilities for each component.
-- Probably the actual data-type is something like [(Double,\Int->a)] or [(Double,[a])] where all the [a] should have the same length.
-- This would be a branch-dependent mixture
data MixtureModels m = MixtureModels (IntMap Int) [Discrete m] IsEqSame

branch_categories (MixtureModels categories _ _) = categories

mmm branch_cats m = MixtureModels branch_cats [m] SameEqs

instance HasAlphabet m => HasAlphabet (MixtureModels m) where
    getAlphabet (MixtureModels _ (m:ms) _) = getAlphabet m

instance HasSMap m => HasSMap (MixtureModels m) where
    getSMap (MixtureModels _ (m:ms) _) = getSMap m

instance CheckReversible m => CheckReversible (MixtureModels m) where
    getReversibility (MixtureModels _ [m] _      ) = getReversibility m
    getReversibility (MixtureModels _ ms  SameEqs) = minimum $ fmap getReversibility ms
    getReversibility (MixtureModels _ _   _      ) = NonEq

instance (HasSMap m, HasAlphabet m, RateModel m, HasBranchLengths t, SimpleSModel t m) => SimpleSModel t (MixtureModels m) where
    type instance IsReversible (MixtureModels m) = IsReversible m
    branchTransitionP (SModelOnTree tree smodel@(MixtureModels branchCats mms _)) b = branchTransitionP (SModelOnTree tree mx) b
        where mx = scaleTo 1 $ mms!!(branchCats IntMap.! b)
    distribution           (SModelOnTree tree (MixtureModels _ (m:ms) _)) = distribution (SModelOnTree tree m)
    nBaseModels            (SModelOnTree tree (MixtureModels _ (m:ms) _)) = nBaseModels (SModelOnTree tree m)
    stateLetters           (SModelOnTree tree (MixtureModels _ (m:ms) _)) = stateLetters (SModelOnTree tree m)
    componentFrequencies   (SModelOnTree tree (MixtureModels _ (m:ms) _)) = componentFrequencies (SModelOnTree tree m)


-- No Attribute
getForeground Nothing = 0
-- Attribute with no value
getForeground (Just Nothing) = 1
-- Attribute with value
getForeground (Just (Just text)) = read (T.unpack text) :: Int

foregroundBranches tree key = edgeAttributes tree (T.pack key) getForeground

{- If this is called with equilibrium models that have different equilibria, we would get the reversibility wrong. -}

-- OK, so if I change this from [Mixture Omega] to Mixture [Omega] or Mixture (\Int -> Omega), how do I apply the function modelFunc to all the omegas?
branchSite fs ws posP posW branchCats modelFunc = MixtureModels branchCats [bgMixture,fgMixture] SameEqs
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


-- A rate of 1 means that we do not rescale it.
instance RateModel m => RateModel (MixtureModels m) where
    rate _ = 1

instance Scalable m => Scalable (MixtureModels m) where
    scaleBy f (MixtureModels categories mixtures r) = MixtureModels categories (scaleBy f <$> mixtures) r
