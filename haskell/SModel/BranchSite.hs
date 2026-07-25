module SModel.BranchSite where

import SModel.Rate
import SModel.MixtureModel
import SModel.BranchModel
import Tree
import qualified Data.IntMap as IntMap
import qualified Data.Text as T

-- No Attribute
getForeground Nothing = 0
-- Attribute with no value
getForeground (Just Nothing) = 1
-- Attribute with value
getForeground (Just (Just text)) = read (T.unpack text) :: Int

foregroundBranches tree key = edgeAttributes tree (T.pack key) getForeground

-- Builds one separately normalized model per numbered branch category and prefixes its properties.
-- The models must share equilibrium frequencies because BranchModel uses those of its first model.
omegaBranchModel branchCats omegas modelFunc =
    case invalidCategories of
      _ | null omegas ->
            error "omegaBranchModel: expected at least one omega"
      category:_ ->
            error ("omegaBranchModel: branch category " ++ show category ++
                   " has no corresponding omega among " ++ show (length omegas) ++ " values")
      [] ->
            makeBranchModel branchCats prefixedModels
  where
    models = [scaleTo 1 (modelFunc omega) | omega <- omegas]
    invalidCategories =
        [category | category <- IntMap.elems branchCats,
                    category < 0 || category >= length models]
    prefix i = T.pack ("branch" ++ show i ++ "-")
    prefixedModels = [(prefix i, model) | (i, model) <- zip [0..] models]

-- Selects the shared null omega or the independently scaled foreground omega.
branchTestForegroundOmega omega omegaRatio branchDifference =
    if branchDifference == 0 then omega else omega * omegaRatio

-- Constructs the two-category branch model used by the branch-difference test.
twoOmegaBranchModel branchCats omega omegaRatio branchDifference modelFunc =
    omegaBranchModel branchCats
      [omega, branchTestForegroundOmega omega omegaRatio branchDifference] modelFunc

-- This construction assumes that modelFunc returns models with the same
-- equilibrium frequencies for every omega.

-- Transposes the background and foreground mixtures into site components,
-- preserving each branch regime's properties under a distinguishing prefix.
branchSite fs ws posP posW branchCats modelFunc =
    Discrete [(makeBranchModel branchCats
                  [(T.pack "background-", background), (T.pack "foreground-", foreground)],
               probability)
             | ((background, probability), (foreground, _)) <- normalizedModels]
  where
    backgroundDist = mkDiscrete (ws ++ [1]) fs
    acceleratedDist = mkDiscrete (repeat posW) fs
    backgroundMixture = modelFunc <$> mix [1-posP, posP] [backgroundDist, backgroundDist]
    foregroundMixture = modelFunc <$> mix [1-posP, posP] [backgroundDist, acceleratedDist]
    normalizedBackground = scaleTo 1 backgroundMixture
    normalizedForeground = scaleTo 1 foregroundMixture
    normalizedModels = zip (unpackDiscrete normalizedBackground) (unpackDiscrete normalizedForeground)

branchSiteTest fs ws posP posW posSelection branchCats modelFunc =
    branchSite fs ws posP posW' branchCats modelFunc
    where posW' = if (posSelection == 1) then posW else 1
