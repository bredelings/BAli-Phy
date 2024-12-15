module SModel.MixtureModel (module SModel.MixtureModel,
                            module Probability.Distribution.Discrete)
                           where

import Bio.Alphabet
import SModel.Simple
import SModel.Rate
import SModel.Frequency
import Probability.Distribution.Discrete -- for mix
import Probability.Dist                  -- for mean
import Tree
import Markov (CTMC(..))

import SModel.ReversibleMarkov

{-
 - NOTE: I should probably change this to ASRV (Discrete m).
 -       Because the BranchSiteMixture also takes a Discrete m, but means something else.
 -}

{-
 - NOTE: 
 -
 -}

-- For mixtures like mixture([hky85,tn93,gtr]), we probably need to mix on the Matrix level, to avoid shared scaling.
mixture ms fs = mix fs ms

-- Create a mixture of mixtures, and then flatten it:
-- parameter_mixture :: Discrete a -> (a -> MixtureModel b) -> MixtureModel b
parameterMixture values modelFn = values >>= modelFn

rateMixture model rates = scale (1/mean rates) $ rates >>= (\rate -> scale rate model)

wfm (Discrete ms) = let freqs = list_to_vector [ getStartFreqs m | (m,p) <- ms]
                        dist =  list_to_vector [p | (m,p) <- ms ]
                    in builtin_weighted_frequency_matrix dist freqs

averageFrequency ms = list_from_vector $ builtin_average_frequency $ wfm ms

plusInv :: Double -> (Discrete ReversibleMarkov) -> (Discrete ReversibleMarkov)
plusInv pInv ms = scale (1/(1-pInv)) $ mix [1 - pInv, pInv] [ms, always $ inv]
    where a  = getAlphabet ms
          pi = averageFrequency ms
          inv = scale 0 $ f81 pi a

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.

instance HasAlphabet m => HasAlphabet (Discrete m) where
    getAlphabet model = getAlphabet $ component model 0

instance HasSMap m => HasSMap (Discrete m) where
    getSMap model = getSMap $ component model 0

instance (HasBranchLengths t, CTMC m, HasSMap m, SimpleSModel t m) => SimpleSModel t (Discrete m) where
    type instance IsReversible (Discrete m) = IsReversible m
    branch_transition_p (SModelOnTree tree model factor) b = concat [ branch_transition_p (SModelOnTree tree component factor) b | (component, _) <- unpackDiscrete model]
    distribution (SModelOnTree tree model factor) = concat [(pr*) <$> distribution (SModelOnTree tree component factor) | (component, pr) <- unpackDiscrete model]
    componentFrequencies (SModelOnTree tree model factor) = concat [componentFrequencies (SModelOnTree tree component factor) | (component,_) <- unpackDiscrete model]
    stateLetters (SModelOnTree _ model _) = getSMap model

instance Scalable a => Scalable (Discrete a) where
    scale x dist = fmap (scale x) dist

instance RateModel a => RateModel (Discrete a) where
    rate d = mean $ fmap rate d


