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
    componentFrequencies   (SModelOnTree tree (MixtureModels _ (m:ms)) f) i = componentFrequencies (SModelOnTree tree m f) i


-- No Attribute
getForeground Nothing = 0
-- Attribute with no value
getForeground (Just Nothing) = 1
-- Attribute with value
getForeground (Just (Just text)) = read (T.unpack text) :: Int

foregroundBranches tree key = edgeAttributes tree (T.pack key) getForeground
