module SModel.MixtureModel where

import SModel.Simple
import SModel.Rate
import SModel.Frequency
import Probability.Distribution.Discrete -- for mix
import Tree

import SModel.ReversibleMarkov

data MixtureModel = MixtureModel [(Double, ReversibleMarkov)]

mixMM fs ms = MixtureModel $ mix fs [m | MixtureModel m <- ms]
scale_MMs rs ms = [scale r m | (r,m) <- zip' rs ms]

mixMixtureModels l dd = MixtureModel (mix l (map unwrapMM dd))

-- For mixtures like mixture([hky85,tn93,gtr]), we probably need to mix on the Matrix level, to avoid shared scaling.
mixture ms fs = mixMM fs ms
-- Note that this scales the models BY rs instead of TO rs.
scaled_mixture ms rs fs = mixMM fs (scale_MMs rs ms)

-- parameter_mixture :: (a -> MixtureModel b) -> [a] -> MixtureModel b
parameter_mixture values model_fn = MixtureModel [ (f*p, m) |(p,x) <- values, let MixtureModel dist = model_fn x, (f,m) <- dist]

-- parameter_mixture_unit :: (a -> ReversibleMarkov) -> [a] -> MixtureModel ReversibleMarkov
parameter_mixture_unit values model_fn = parameter_mixture values (unit_mixture . model_fn)

rate_mixture m d = parameter_mixture d (\x->scale x m)

average_frequency (MixtureModel ms) = list_from_vector $ builtin_average_frequency $ weighted_frequency_matrix $ MixtureModel ms

extend_mixture (MixtureModel ms) (p,x) = MixtureModel $ mix [p, 1.0-p] [certainly x, ms]

plus_inv p_inv mm = extend_mixture mm (p_inv, scale 0.0 $ f81 pi a)
    where a  = getAlphabet mm
          pi = average_frequency mm

rate_mixture_unif_bins base dist n_bins = rate_mixture base $ uniformDiscretize dist n_bins

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
baseModel (MixtureModel l) i = snd (l !! i)

unwrapMM (MixtureModel dd) = dd

instance SimpleSModel MixtureModel where
    get_smap (MixtureModel ((_,m):_)) = get_smap m
    branch_transition_p (SingleBranchLengthModel tree smodel@(MixtureModel cs)) b = [qExp $ scale (branch_length tree b/r) component | (_,component) <- cs]
        where r = rate smodel
    distribution (MixtureModel l) = map fst l
    weighted_frequency_matrix (MixtureModel d) = let model = MixtureModel d
                                                     dist = list_to_vector $ distribution model
                                                     freqs = list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]
                                                 in builtin_weighted_frequency_matrix dist freqs
    frequency_matrix (MixtureModel d) = let model = MixtureModel d
                                        in  builtin_frequency_matrix $ list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]
    nBaseModels (MixtureModel l) = length l
    stateLetters (MixtureModel l) = stateLetters (baseModel (MixtureModel l) 0)
    getAlphabet (MixtureModel l) = getAlphabet (baseModel (MixtureModel l) 0)
    componentFrequencies (MixtureModel d) i = frequencies (baseModel (MixtureModel d) i)


instance RateModel MixtureModel where
    rate (MixtureModel d) = average [(p,rate m) | (p,m) <- d]
    scale x (MixtureModel dist              ) = MixtureModel [(p, scale x m) | (p, m) <- dist]


---

unit_mixture m = MixtureModel (certainly m)

