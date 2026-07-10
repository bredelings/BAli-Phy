module SModel.BranchModel where

import qualified Markov
import           Markov (CTMC(..))
import           Reversible
import           SModel.Simple
import           SModel.Property
import           SModel.Rate
import           SModel.Frequency
import           Bio.Alphabet
import           Numeric.LinearAlgebra
import           Tree
import qualified Data.Map as Map

{-
  This model is for PAML's branch-model, where every branch can have a different Q matrix
  BUT they must all have the same equilibrium frequencies.
 -}

-- Should this also take a tree?
-- Should we just have a bare function?
-- Should we have an IntMap?
data BranchMap a = BranchMap (Int -> a)

data BranchModel a = BranchModel Alphabet (EVector Int) (Vector Double) (BranchMap a)

instance Functor BranchMap where
    fmap f (BranchMap g) = BranchMap (f . g)

instance Functor BranchModel where
    fmap f (BranchModel a smap pi map) = BranchModel a smap pi (f <$> map)

instance HasAlphabet (BranchModel a) where
    getAlphabet (BranchModel alphabet _ _ _) = alphabet

instance HasSMap (BranchModel a) where
    getSMap (BranchModel _ smap _ _) = smap

instance CheckReversible (BranchModel m) where
    getReversibility _ = NonEq

instance (HasSMap m, HasBranchLengths t, CTMC m) => SimpleSModel t (BranchModel m) where
    stateLetters (SModelOnTree tree model) = getSMap model
    branchTransitionP (SModelOnTree tree model) b = [qExp $ scaleBy (branchLength tree b) (ratesForBranch b)]
        where (BranchModel _ _ _ (BranchMap ratesForBranch)) = model
    componentFrequencies (SModelOnTree _ (BranchModel _ _ pi _)) = [pi]

-- Branch-specific rate models need branch-indexed property semantics, so the
-- homogeneous component/state property map is empty for now.
instance HasProperties t (BranchModel m) where
    getProperties _ = Map.empty
