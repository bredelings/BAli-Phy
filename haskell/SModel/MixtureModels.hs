module SModel.MixtureModels where

import SModel.Simple
import SModel.MixtureModel
import Tree
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Text as T

-- Currently we are weirdly duplicating the mixture probabilities for each component.
-- Probably the actual data-type is something like [(Double,\Int->a)] or [(Double,[a])] where all the [a] should have the same length.
-- This would be a branch-dependent mixture
data MixtureModels = MixtureModels (IntMap Int) [MixtureModel]

branch_categories (MixtureModels categories _) = categories

mmm branch_cats m = MixtureModels branch_cats [m]

instance SimpleSModel MixtureModels where
    branch_transition_p (SingleBranchLengthModel tree smodel@(MixtureModels branchCats mms)) b = branch_transition_p (SingleBranchLengthModel tree mx) b
        where mx = mms!!(branchCats IntMap.! undirectedName tree b)
    distribution              (MixtureModels _ (m:ms)) = distribution m
    weighted_frequency_matrix (MixtureModels _ (m:ms)) = weighted_frequency_matrix m
    frequency_matrix          (MixtureModels _ (m:ms)) = frequency_matrix m
    nBaseModels               (MixtureModels _ (m:ms)) = nBaseModels m
    stateLetters              (MixtureModels _ (m:ms)) = stateLetters m
    getAlphabet               (MixtureModels _ (m:ms)) = getAlphabet m
    componentFrequencies      (MixtureModels _ (m:ms)) i = componentFrequencies m i


-- No Attribute
getForeground Nothing = 0
-- Attribute with no value
getForeground (Just Nothing) = 1
-- Attribute with value
getForeground (Just (Just text)) = read (T.unpack text) :: Int

foregroundBranches tree key = edgeAttributes tree (T.pack key) getForeground
