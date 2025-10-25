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
import           Reversible

{- NOTE: As branch lengths approach 0, this approaches a CTMC model with Q = meanMatrix Qs -}

data BranchSiteMixture m = BranchSiteMixture (Discrete m) IsEqSame

instance Scalable m => Scalable (BranchSiteMixture m) where
    scaleBy x (BranchSiteMixture d e) = BranchSiteMixture (scaleBy x d) e

-- The model rate is the average of the Q matrix rates.
instance RateModel m => RateModel (BranchSiteMixture m) where
    rate (BranchSiteMixture d _) = rate d

instance HasAlphabet m => HasAlphabet (BranchSiteMixture m) where
    getAlphabet (BranchSiteMixture d _) = getAlphabet d

instance HasSMap m => HasSMap (BranchSiteMixture m) where
    getSMap (BranchSiteMixture d _) = getSMap q
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


{-
  If there is a single model, then the reversibility and stationarity are the same as the single model.
  If we claim that all the models have the same equilibrium then the total is EqRev if all of them are, and EqNonRev, if all of them are, and NonEq otherwise.
  If any of the models is NonEq, then this is NonEq.

  The starting frequencies for this model should be the weighted average of the starting frequencies for the individual models.
  If they are all at equilibrium, and the equilibriums are all the same, then that's just the starting frequency for one model.
-}

instance CheckReversible m => CheckReversible (BranchSiteMixture m) where
    getReversibility (BranchSiteMixture (Discrete [(m,_)]) _      ) = getReversibility m
    getReversibility (BranchSiteMixture ms                 SameEqs) = getReversibility ms
    getReversibility _                                             = NonEq

instance (HasSMap m, CTMC m, HasAlphabet m, HasBranchLengths t, SimpleSModel t m) => SimpleSModel t (BranchSiteMixture m) where
    branchTransitionP (SModelOnTree tree (BranchSiteMixture m r)) b = [ meanMatrix $ (qExp . scaleBy (branchLength tree b)) <$> m ]
    distribution _ = [1]
    stateLetters (SModelOnTree _ smodel) = getSMap model
        where BranchSiteMixture (Discrete models) _ = smodel
              (model,_):_ = models
    componentFrequencies (SModelOnTree _ smodel) = [getStartFreqs model]
        where BranchSiteMixture (Discrete models) _ = smodel
              (model,_):_ = models
