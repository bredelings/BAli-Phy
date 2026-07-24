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

{-
  This model is for PAML's branch-model, where every branch can have a different Q matrix
  BUT they must all have the same equilibrium frequencies.
 -}

-- Selects one same-equilibrium model per branch category and tracks its common
-- scale and reportable component properties.
data BranchModel m = BranchModel (IntMap Int) [m] Double StatePropertyMap

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
    getStatePropertyFunctions (BranchModel _ _ _ properties) = properties
    setStateProperty name property (BranchModel categories models modelRate properties) =
        BranchModel categories models modelRate (Map.insert name property properties)
    nPropertyStates model = vector_size (getSMap model)

instance HasSMap m => HasProperties t (BranchModel m) where
    getProperties (SModelOnTree _ model) =
        statePropertyMapToComponentPropertyMap $ getStateProperties model

instance Scalable m => Scalable (BranchModel m) where
    scaleBy factor (BranchModel categories models modelRate properties) =
        BranchModel categories (scaleBy factor <$> models) (factor * modelRate)
                    (scaleStatePropertyMap factor properties)

instance Scalable m => RateModel (BranchModel m) where
    rate (BranchModel _ _ modelRate _) = modelRate
