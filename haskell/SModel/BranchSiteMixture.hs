module SModel.BranchSiteMixture where

import           Probability.Distribution.Discrete
import           Data.Matrix
import           Bio.Alphabet (HasSMap(..), HasAlphabet(..))
import           Graph
import qualified Data.Matrix as M
import           SModel.Rate
import           SModel.Simple
import           Markov (CTMC(..), qExp)
import           SModel.MixtureModel ( ) -- just for the instances

data BranchSiteMixture m = BranchSiteMixture (Discrete m)

instance Scalable m => Scalable (BranchSiteMixture m) where
    scaleBy x (BranchSiteMixture d) = BranchSiteMixture (scaleBy x d)

-- The model rate is the average of the Q matrix rates.
instance RateModel m => RateModel (BranchSiteMixture m) where
    rate (BranchSiteMixture d) = rate d

instance HasAlphabet m => HasAlphabet (BranchSiteMixture m) where
    getAlphabet (BranchSiteMixture d) = getAlphabet d

instance HasSMap m => HasSMap (BranchSiteMixture m) where
    getSMap (BranchSiteMixture d) = getSMap q
        where Discrete ((q,_):_) = d

{- How should we scale the rate matrices?
   All Q matrices should be scaled by the same amount.

   The BUSTED paper does not specify a rescaling factor for the Q matrices.
   However, it says that branch lengths are fixed after optimization under a GTR model.
   Thus it makes sense to define the average rate to be 1.

   Currently (Dec 2024) phyloCTMC sets the average rate to be 1, so we do not need to
     divide by the model rate in branchTransitionP.
 -}

meanMatrix (Discrete mps) = foldl1 (%+%) [scaleMatrix p m | (m,p) <- mps]

instance (HasSMap m, CTMC m, HasAlphabet m, HasBranchLengths t, SimpleSModel t m) => SimpleSModel t (BranchSiteMixture m) where
    type instance IsReversible (BranchSiteMixture m) = IsReversible m
    branchTransitionP (SModelOnTree tree (BranchSiteMixture m)) b = [ meanMatrix $ (qExp . scaleBy (branchLength tree b)) <$> m ]
    distribution _ = [1]
    stateLetters (SModelOnTree _ smodel) = getSMap model
        where BranchSiteMixture (Discrete models) = smodel
              (model,_):_ = models
    componentFrequencies (SModelOnTree _ smodel) = [getStartFreqs model]
        where BranchSiteMixture (Discrete models) = smodel
              (model,_):_ = models
