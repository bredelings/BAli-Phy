module SModel.MixtureModels where

import SModel.Rate
import SModel.MixtureModel
import SModel.BranchModel
import SModel.Property
import Tree
import qualified Data.Map as Map
import qualified Data.Text as T

-- No Attribute
getForeground Nothing = 0
-- Attribute with no value
getForeground (Just Nothing) = 1
-- Attribute with value
getForeground (Just (Just text)) = read (T.unpack text) :: Int

foregroundBranches tree key = edgeAttributes tree (T.pack key) getForeground

-- This construction assumes that modelFunc returns models with the same
-- equilibrium frequencies for every omega.

-- Transposes the background and foreground mixtures into site components,
-- preserving each branch regime's properties under a distinguishing prefix.
branchSite fs ws posP posW branchCats modelFunc =
    Discrete [(BranchModel branchCats [background, foreground] 1
                  (Map.union
                    (Map.mapKeys (T.append (T.pack "background-")) $
                       getStatePropertyFunctions background)
                    (Map.mapKeys (T.append (T.pack "foreground-")) $
                       getStatePropertyFunctions foreground)),
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
