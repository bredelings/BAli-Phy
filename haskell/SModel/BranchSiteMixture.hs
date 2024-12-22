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
    scaleBy x (BranchSiteMixture d) = BranchSiteMixture (scaleBy x d)

instance RateModel m => RateModel (BranchSiteMixture m) where
    rate (BranchSiteMixture d) = rate d

instance HasAlphabet m => HasAlphabet (BranchSiteMixture m) where
    getAlphabet (BranchSiteMixture d) = getAlphabet d

instance HasSMap m => HasSMap (BranchSiteMixture m) where
    getSMap (BranchSiteMixture d) = getSMap q
        where Discrete ((q,_):_) = d

instance (HasSMap m, CTMC m, HasAlphabet m, HasBranchLengths t, SimpleSModel t m) => SimpleSModel t (BranchSiteMixture m) where
    type instance IsReversible (BranchSiteMixture m) = IsReversible m
    branchTransitionP (SModelOnTree tree smodel) b =
        [ foldl1 (%+%) [scaleMatrix p $ qExp $ scaleBy (branchLength tree b) q | (q,p) <- models] ]
            where BranchSiteMixture (Discrete models) = smodel
    distribution _ = [1]
    stateLetters (SModelOnTree _ smodel) = getSMap model
        where BranchSiteMixture (Discrete models) = smodel
              (model,_):_ = models
    componentFrequencies (SModelOnTree _ smodel) = [getStartFreqs model]
        where BranchSiteMixture (Discrete models) = smodel
              (model,_):_ = models
