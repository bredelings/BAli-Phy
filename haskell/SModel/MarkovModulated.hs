module SModel.MarkovModulated where

import Foreign.Vector
import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.MixtureModel
import SModel.Rate
import Data.Matrix -- for fromLists

foreign import bpcall "SModel:modulated_markov_rates" builtin_modulated_markov_rates :: EVector (Matrix Double) -> Matrix Double -> Matrix Double
foreign import bpcall "SModel:modulated_markov_pi" builtin_modulated_markov_pi :: EVector (EVector Double) -> EVector Double -> EVector Double
foreign import bpcall "SModel:modulated_markov_smap" builtin_modulated_markov_smap :: EVector (EVector Int) -> EVector Int

modulated_markov_rates qs rates_between = builtin_modulated_markov_rates (list_to_vector qs) rates_between

modulated_markov_pi pis level_probs = builtin_modulated_markov_pi (list_to_vector pis) (list_to_vector level_probs)

modulated_markov_smap smaps = builtin_modulated_markov_smap (list_to_vector smaps)

-- This could get renamed, after I look at the paper that uses the term "modulated markov"
modulated_markov models rates_between level_probs = reversible_markov a smap q pi where
    a = get_alphabet $ head models
    qs = map get_q models
    pis = map get_pi models
    smaps = map get_smap' models
    q = modulated_markov_rates qs rates_between
    pi = modulated_markov_pi pis level_probs
    smap = modulated_markov_smap smaps

markov_modulate_mixture nu (MixtureModel dist) = modulated_markov models rates_between level_probs where
    (level_probs,models) = unzip dist
    rates_between = generic_equ (length models) nu

-- We need to rescale submodels to have substitution rate `1.0`.
-- Otherwise class-switching rates are not relative to the substitution rate.

tuffley_steel_98_unscaled s01 s10 q = modulated_markov [scale 0.0 q, q] rates_between level_probs where
    level_probs = [s10/total, s01/total] where total = s10 + s01
    rates_between = fromLists [[-s01,s01],[s10,-s10]]

tuffley_steel_98 s01 s10 q = tuffley_steel_98_unscaled s01 s10 (rescale 1.0 q)

huelsenbeck_02 s01 s10 model = MixtureModel [(p, tuffley_steel_98_unscaled s01 s10 q) | (p,q) <- dist] where
    MixtureModel dist = rescale 1.0 model

galtier_01_ssrv nu model = modulated_markov models rates_between level_probs where
    MixtureModel dist = rescale 1.0 model
    level_probs = map fst dist
    models = map snd dist
    n_levels = length dist
    -- This is really a generic gtr...  We should be able to get this with f81
    rates_between = (generic_equ n_levels nu) %*% (plus_f_matrix $ list_to_vector level_probs)

galtier_01 nu pi model = parameter_mixture_unit [(1.0-pi, 0.0), (pi, nu)] (\nu' -> galtier_01_ssrv nu' model)

wssr07_ssrv s01 s10 nu model = tuffley_steel_98 s01 s10 $ galtier_01_ssrv nu model

wssr07 s01 s10 nu pi model = parameter_mixture_unit [(1.0-pi, 0.0), (pi, nu)] (\nu' -> wssr07_ssrv s01 s10 nu' model)

