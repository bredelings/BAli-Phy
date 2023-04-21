module SModel.MixtureModel (module SModel.MixtureModel,
                            module Probability.Distribution.Discrete)
                           where

import SModel.Simple
import SModel.Rate
import SModel.Frequency
import Probability.Distribution.Discrete -- for mix
import Probability.Dist                  -- for mean
import Tree
import Markov (qExp)

import SModel.ReversibleMarkov

data MixtureModel = MixtureModel (Discrete ReversibleMarkov)

mixMM fs ms = MixtureModel $ mix fs [m | MixtureModel m <- ms]
scale_MMs rs ms = [scale r m | (r,m) <- zip' rs ms]

mixMixtureModels l dd = MixtureModel (mix l (map unwrapMM dd))

-- For mixtures like mixture([hky85,tn93,gtr]), we probably need to mix on the Matrix level, to avoid shared scaling.
mixture ms fs = mixMM fs ms
-- Note that this scales the models BY rs instead of TO rs.
scaled_mixture ms rs fs = mixMM fs (scale_MMs rs ms)

-- parameter_mixture :: Discrete a -> (a -> MixtureModel b) -> MixtureModel b
parameter_mixture values model_fn = MixtureModel $ Discrete [ (m, f*p) | (x, p) <- unpackDiscrete values,
                                                                         let MixtureModel dist = model_fn x,
                                                                         (m, f) <- unpackDiscrete dist]

-- parameter_mixture_unit :: (a -> ReversibleMarkov) -> [a] -> MixtureModel ReversibleMarkov
parameter_mixture_unit values model_fn = parameter_mixture values (unit_mixture . model_fn)

rate_mixture m d = parameter_mixture d (\x->scale x m)

average_frequency (MixtureModel ms) = list_from_vector $ builtin_average_frequency $ weighted_frequency_matrix $ MixtureModel ms

extend_mixture (MixtureModel ms) (x,p) = MixtureModel $ mix [p, 1-p] [certainly x, ms]

plus_inv p_inv mm = extend_mixture mm (scale 0 $ f81 pi a, p_inv)
    where a  = getAlphabet mm
          pi = average_frequency mm

rate_mixture_unif_bins base dist n_bins = rate_mixture base $ uniformDiscretize dist n_bins

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
baseModel (MixtureModel (Discrete l)) i = fst (l !! i)

unwrapMM (MixtureModel dd) = dd

instance SimpleSModel MixtureModel where
    branch_transition_p (SingleBranchLengthModel tree smodel@(MixtureModel cs)) b = [qExp $ scale (branch_length tree b/r) component | (component,_) <- unpackDiscrete cs]
        where r = rate smodel
    distribution (MixtureModel l) = map snd (unpackDiscrete l)
    weighted_frequency_matrix (MixtureModel d) = let model = MixtureModel d
                                                     dist = list_to_vector $ distribution model
                                                     freqs = list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]
                                                 in builtin_weighted_frequency_matrix dist freqs
    frequency_matrix (MixtureModel d) = let model = MixtureModel d
                                        in  builtin_frequency_matrix $ list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]
    nBaseModels (MixtureModel l) = length $ unpackDiscrete l
    stateLetters (MixtureModel l) = stateLetters (baseModel (MixtureModel l) 0)
    getAlphabet (MixtureModel l) = getAlphabet (baseModel (MixtureModel l) 0)
    componentFrequencies (MixtureModel d) i = frequencies (baseModel (MixtureModel d) i)


instance Scalable MixtureModel where
    scale x (MixtureModel dist) = MixtureModel $ fmap (scale x) dist

instance RateModel MixtureModel where
    rate (MixtureModel d) = mean $ fmap rate d


---

unit_mixture m = MixtureModel (certainly m)

