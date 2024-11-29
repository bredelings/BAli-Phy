module SModel.BranchSiteMixture where

import           Probability.Distribution.Discrete
import           Data.Matrix
import           Bio.Alphabet (HasSMap(..), HasAlphabet(..))
import           Graph
import qualified Data.Matrix as M
import           SModel.Rate
import           SModel.Simple
import           Markov (CTMC(..), qExp)
import           SModel.MixtureModel -- just for the instances

data BranchSiteMixture m = BranchSiteMixture (Discrete m)

instance Scalable m => Scalable (BranchSiteMixture m) where
    scale x (BranchSiteMixture d) = BranchSiteMixture (scale x d)

instance RateModel m => RateModel (BranchSiteMixture m) where
    rate (BranchSiteMixture d) = rate d

instance HasAlphabet m => HasAlphabet (BranchSiteMixture m) where
    getAlphabet (BranchSiteMixture d) = getAlphabet d

instance (HasSMap m, CTMC m, HasAlphabet m, RateModel m, HasBranchLengths t, SimpleSModel t m) => SimpleSModel t (BranchSiteMixture m) where
    type instance IsReversible (BranchSiteMixture m) = IsReversible m
    branch_transition_p (SModelOnTree tree smodel factor) b =
        [ foldl1 (%+%) [scaleMatrix p $ qExp $ scale (factor * branchLength tree b / r) q | (q,p) <- models] ]
            where BranchSiteMixture (Discrete models) = smodel
                  r = rate smodel
    distribution _ = [1]
    stateLetters (SModelOnTree _ smodel _) = getSMap model
        where BranchSiteMixture (Discrete models) = smodel
              (model,_):_ = models
    componentFrequencies (SModelOnTree _ smodel _) i = [getStartFreqs model]!!i
        where BranchSiteMixture (Discrete models) = smodel
              (model,_):_ = models
