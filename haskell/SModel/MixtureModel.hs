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

scale_MMs rs ms = [scale r m | (r,m) <- zip' rs ms]

-- For mixtures like mixture([hky85,tn93,gtr]), we probably need to mix on the Matrix level, to avoid shared scaling.
mixture ms fs = mix fs ms

-- Note that this scales the models BY rs instead of TO rs.
scaled_mixture ms rs fs = mix fs (scale_MMs rs ms)

-- parameter_mixture :: Discrete a -> (a -> MixtureModel b) -> MixtureModel b
parameter_mixture values model_fn = Discrete [ (m, f*p) | (x, p) <- unpackDiscrete values,
                                                          let dist =  model_fn x,
                                                          (m, f) <- unpackDiscrete dist]

-- parameter_mixture_unit :: (a -> ReversibleMarkov) -> [a] -> MixtureModel ReversibleMarkov
parameter_mixture_unit values model_fn = parameter_mixture values (unit_mixture . model_fn)

rate_mixture m d = parameter_mixture d (\x->scale x m)

wfm (Discrete ms) = let freqs = list_to_vector [ getStartFreqs m | (m,p) <- ms]
                        dist =  list_to_vector [p | (m,p) <- ms ]
                    in builtin_weighted_frequency_matrix dist freqs

averageFrequency ms = list_from_vector $ builtin_average_frequency $ wfm ms

plus_inv :: Double -> (Discrete ReversibleMarkov) -> (Discrete ReversibleMarkov)
plus_inv pInv ms = addComponent ms (scale 0 $ f81 pi a, pInv)
    where a  = getAlphabet ms
          pi = averageFrequency ms

rate_mixture_unif_bins base dist n_bins = rate_mixture base $ uniformDiscretize dist n_bins

-- If we had a mixture of mixtures.
baseModel model i = component model i

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.

instance HasAlphabet m => HasAlphabet (Discrete m) where
    getAlphabet model = getAlphabet $ baseModel model 0

instance HasSMap m => HasSMap (Discrete m) where
    getSMap model = getSMap $ baseModel model 0

instance (HasBranchLengths t, CTMC m, HasSMap m, RateModel m, SimpleSModel t m) => SimpleSModel t (Discrete m) where
    type instance IsReversible (Discrete m) = IsReversible m
    branch_transition_p (SModelOnTree tree model factor) b = [qExp $ scale (factor * branchLength tree b / r) component | (component,_) <- unpackDiscrete model]
        where r = rate model
    distribution (SModelOnTree _ model _) = map snd (unpackDiscrete model)
    componentFrequencies (SModelOnTree _ model _) i = getStartFreqs $ baseModel model i
    stateLetters (SModelOnTree _ model _) = getSMap model

instance Scalable a => Scalable (Discrete a) where
    scale x dist = fmap (scale x) dist

instance RateModel a => RateModel (Discrete a) where
    rate d = mean $ fmap rate d


