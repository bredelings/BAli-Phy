module SModel.MarkovModulated where

import Foreign.Vector
import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.MixtureModel
import SModel.Rate
import qualified Markov
import Markov (getQ, getPi)
import Data.Matrix -- for fromLists, %*%

foreign import bpcall "SModel:modulated_markov_rates" builtin_modulated_markov_rates :: EVector (Matrix Double) -> Matrix Double -> Matrix Double
foreign import bpcall "SModel:modulated_markov_pi" builtin_modulated_markov_pi :: EVector (EVector Double) -> EVector Double -> EVector Double
foreign import bpcall "SModel:modulated_markov_smap" builtin_modulated_markov_smap :: EVector (EVector Int) -> EVector Int

modulated_markov_rates qs rates_between = builtin_modulated_markov_rates (list_to_vector qs) rates_between

modulated_markov_pi pis level_probs = builtin_modulated_markov_pi (list_to_vector pis) (list_to_vector level_probs)

modulated_markov_smap smaps = builtin_modulated_markov_smap (list_to_vector smaps)

-- This could get renamed, after I look at the paper that uses the term "modulated markov"
modulated_markov models rates_between level_probs = reversible_markov a smap q pi where
    a = getAlphabet $ head models
    qs = map getQ models
    pis = map getPi models
    smaps = map get_smap models
    q = modulated_markov_rates qs rates_between
    pi = modulated_markov_pi pis level_probs
    smap = modulated_markov_smap smaps

markov_modulate_mixture nu dist = modulated_markov models rates_between level_probs where
    (models, level_probs) = unzip $ unpackDiscrete dist
    rates_between = Markov.equ (length models) nu

-- We need to rescale submodels to have substitution rate `1`.
-- Otherwise class-switching rates are not relative to the substitution rate.

tuffley_steel_98_unscaled s01 s10 q = modulated_markov [scale 0 q, q] rates_between level_probs where
    level_probs = [s10/total, s01/total] where total = s10 + s01
    rates_between = fromLists [[-s01,s01],[s10,-s10]]

tuffley_steel_98 s01 s10 q = tuffley_steel_98_unscaled s01 s10 (rescale 1 q)

tuffley_steel_98_test s01 s10 fraction q = mix [1-fraction, fraction] [tuffley_steel_98_unscaled 1 0 (rescale 1 q) & unit_mixture,
                                                                       tuffley_steel_98_unscaled s01 s10 (rescale 1 q) & unit_mixture]

huelsenbeck_02 s01 s10 model = fmap (tuffley_steel_98_unscaled s01 s10) (rescale 1 model)

huelsenbeck_02_test s01 s10 fraction model = mix [1-fraction, fraction] [model & huelsenbeck_02 1 0,
                                                                         -- ^ ideally we could just put "model" here.
                                                                         model & huelsenbeck_02 s01 s10]

galtier_01_ssrv nu model = modulated_markov models rates_between level_probs where
    dist = rescale 1 model
    (models, level_probs) = unzip $ unpackDiscrete dist
    n_levels = length models
    -- This is really a generic gtr...  We should be able to get this with f81
    rates_between = (Markov.equ n_levels nu) %*% (plus_f_matrix $ list_to_vector level_probs)

galtier_01 nu pi model = parameter_mixture_unit (Discrete [(0, 1-pi), (nu, pi)]) (\nu' -> galtier_01_ssrv nu' model)

wssr07_ssrv s01 s10 nu model = tuffley_steel_98 s01 s10 $ galtier_01_ssrv nu model

wssr07 s01 s10 nu pi model = parameter_mixture_unit (Discrete [(0, 1-pi), (nu, pi)]) (\nu' -> wssr07_ssrv s01 s10 nu' model)

-- a -> HB02
-- b -> GT01 if no HB02
-- c -> GT01 if    HB02

-- X = (1-a)(1-b)
-- X + HB02 = a(1-c)
-- X + GT01 = (1-a)b
-- X + HB02+GT01 = a * c

wssr07Ext s01 s10 nu a b c model = Discrete [(noCov,   (1-a)*(1-b)),
                                             (gt01,    (1-a)*b),
                                             (hb02,        a*(1-c)),
                                             (hb02gt01,    a*c)]
    where noCov = wssr07_ssrv 1 0 0  model
          gt01  = wssr07_ssrv 1 0 nu model
          hb02  = wssr07_ssrv s01 s10 0 model
          hb02gt01 = wssr07_ssrv s01 s10 nu model

-- Instead of passing rates_between+level_probs, could we just pass a q matrix?
covarion_gtr_ssrv nu exchange model = modulated_markov models rates_between level_probs where
    Discrete dist = rescale 1 model
    (models, level_probs) = unzip dist
    -- This is really a gtr rate matrix, just without the alphabet / smap!
    rates_between = (scaleMatrix nu exchange) %*% (plus_f_matrix $ list_to_vector level_probs)

covarion_gtr nu exchange pi model = parameter_mixture_unit (Discrete [(0,1-pi), (nu, pi)]) (\nu' -> covarion_gtr_ssrv nu' exchange model)

covarion_gtr_sym :: Matrix Double -> Discrete ReversibleMarkov -> ReversibleMarkov
covarion_gtr_sym sym model = modulated_markov models rates_between level_probs where
    dist = rescale 1 model
    (models, level_probs) = unzip $ unpackDiscrete dist
    rates_between = sym %*% (plus_f_matrix $ list_to_vector level_probs)


