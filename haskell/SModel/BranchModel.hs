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

-- Selects one same-equilibrium rate model for each branch category and tracks
-- the common scale applied after the category models were normalized.
data BranchModel m = BranchModel (IntMap Int) [m] Double

instance HasAlphabet m => HasAlphabet (BranchModel m) where
    getAlphabet (BranchModel _ (model:_) _) = getAlphabet model

instance HasSMap m => HasSMap (BranchModel m) where
    getSMap (BranchModel _ (model:_) _) = getSMap model

instance CheckReversible m => CheckReversible (BranchModel m) where
    getReversibility (BranchModel _ models _) = minimum $ fmap getReversibility models

instance (HasSMap m, HasBranchLengths t, CTMC m, CheckReversible m) => SimpleSModel t (BranchModel m) where
    stateLetters (SModelOnTree _ model) = getSMap model
    branchTransitionP (SModelOnTree tree (BranchModel categories models _)) b =
        [qExp $ scaleBy (branchLength tree b) (models !! (categories IntMap.! b))]
    componentFrequencies (SModelOnTree _ (BranchModel _ (model:_) _)) = [getStartFreqs model]

-- Branch-specific rate models need branch-indexed property semantics, so the
-- homogeneous component/state property map is empty for now.
instance HasProperties t (BranchModel m) where
    getProperties _ = Map.empty

instance Scalable m => Scalable (BranchModel m) where
    scaleBy factor (BranchModel categories models modelRate) =
        BranchModel categories (scaleBy factor <$> models) (factor * modelRate)

instance Scalable m => RateModel (BranchModel m) where
    rate (BranchModel _ _ modelRate) = modelRate
