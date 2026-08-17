module SModel.BranchModel where

import qualified Markov
import           Markov (CTMC(..))
import           Reversible
import           SModel.Simple
import           SModel.Property
import           SModel.Rate
import           Bio.Alphabet
import           Tree
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as T

{-
  This model is for PAML's branch-model, where every branch can have a different Q matrix
  BUT they must all have the same equilibrium frequencies.
 -}

-- Selects one same-equilibrium model per branch category and tracks its common
-- scale and reportable component properties.
data BranchModel m = BranchModel (IntMap Int) [m] Double ComponentAnnotations

-- Builds a branch model and preserves each category model's properties under its supplied prefix.
-- The category models must share equilibrium frequencies.
makeBranchModel categories prefixedModels =
    BranchModel categories models 1 (ComponentAnnotations properties conditions)
  where
    models = fmap snd prefixedModels
    properties =
        foldr Map.union Map.empty
          [Map.mapKeys (T.append prefix) (getStatePropertyFunctions model)
          | (prefix, model) <- prefixedModels]
    conditions = commonOrConditionMap [getComponentConditions model | (_, model) <- prefixedModels]

instance HasAlphabet m => HasAlphabet (BranchModel m) where
    getAlphabet (BranchModel _ (model:_) _ _) = getAlphabet model

instance HasSMap m => HasSMap (BranchModel m) where
    getSMap (BranchModel _ (model:_) _ _) = getSMap model

instance CheckReversible m => CheckReversible (BranchModel m) where
    getReversibility (BranchModel _ models _ _) = minimum $ fmap getReversibility models

instance (HasSMap m, HasBranchLengths t, CTMC m, CheckReversible m) => SimpleSModel t (BranchModel m) where
    stateLetters (SModelOnTree _ model) = getSMap model
    branchTransitionP (SModelOnTree tree (BranchModel categories models _ _)) b =
        [qExp $ scaleBy (branchLength tree b) (models !! (categories IntMap.! b))]
    componentFrequencies (SModelOnTree _ (BranchModel _ (model:_) _ _)) = [getStartFreqs model]

instance HasSMap m => HasStateProperties (BranchModel m) where
    getStatePropertyFunctions (BranchModel _ _ _ (ComponentAnnotations properties _)) = properties
    setStateProperty name property
        (BranchModel categories models modelRate (ComponentAnnotations properties conditions)) =
        BranchModel categories models modelRate (ComponentAnnotations (Map.insert name property properties) conditions)
    nPropertyStates model = vector_size (getSMap model)

instance HasComponentConditions (BranchModel m) where
    getComponentConditions (BranchModel _ _ _ (ComponentAnnotations _ conditions)) = conditions
    setComponentCondition name value
        (BranchModel categories models modelRate (ComponentAnnotations properties conditions)) =
            BranchModel categories models modelRate
                (ComponentAnnotations properties (Map.insert name value conditions))

instance HasSMap m => HasProperties t (BranchModel m) where
    getProperties (SModelOnTree _ model) =
        statePropertyMapToComponentPropertyMap $ getStateProperties model
    getConditions (SModelOnTree _ model) = getComponentConditions model

instance Scalable m => Scalable (BranchModel m) where
    scaleBy factor (BranchModel categories models modelRate annotations) =
        BranchModel categories (scaleBy factor <$> models) (factor * modelRate)
                    (scaleComponentAnnotations factor annotations)

instance Scalable m => RateModel (BranchModel m) where
    rate (BranchModel _ _ modelRate _) = modelRate
