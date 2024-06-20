module SModel.BranchModel where

import qualified Markov
import           Markov (CTMC(..))
import           SModel.Simple
import           SModel.Rate
import           SModel.Frequency
import           Bio.Alphabet
import           Data.Matrix
import           Tree

-- Should this also take a tree?
-- Should we just have a bare function?
-- Should we have an IntMap?
data BranchMap a = BranchMap (Int -> a)

data BranchModel a = BranchModel Alphabet (EVector Int) (EVector Double) (BranchMap a)

instance Functor BranchMap where
    fmap f (BranchMap g) = BranchMap (f . g)

instance Functor BranchModel where
    fmap f (BranchModel a smap pi map) = BranchModel a smap pi (f <$> map)

instance HasAlphabet (BranchModel a) where
    getAlphabet (BranchModel alphabet _ _ _) = alphabet

instance CTMC a => SimpleSModel (BranchModel a) where
    stateLetters (BranchModel _ smap _ _) = smap
    branch_transition_p (SingleBranchLengthModel tree model f) b = [qExp $ scale (branch_length tree b * f) (ratesForBranch b)]
        where (BranchModel _ _ _ (BranchMap ratesForBranch)) = model
    distribution _ = [1]
    nBaseModels _ = 1
    componentFrequencies (BranchModel _ _ pi _) i = [pi] !! i
