module SModel.MarkovModulated where

import Foreign.Vector
import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.MixtureModel
import SModel.Rate
import qualified Markov
import Markov (getQ, getEqFreqs)
import Data.Matrix -- for fromLists, %*%

foreign import bpcall "SModel:modulated_markov_rates" builtin_modulated_markov_rates :: EVector (Matrix Double) -> Matrix Double -> Matrix Double
foreign import bpcall "SModel:modulated_markov_pi" builtin_modulated_markov_pi :: EVector (EVector Double) -> EVector Double -> EVector Double
foreign import bpcall "SModel:modulated_markov_smap" builtin_modulated_markov_smap :: EVector (EVector Int) -> EVector Int

modulatedMarkovRates qs rates_between = builtin_modulated_markov_rates (toVector qs) rates_between

modulatedMarkovPi pis levelProbs = builtin_modulated_markov_pi (toVector pis) (toVector levelProbs)

modulatedMarkovSmap smaps = builtin_modulated_markov_smap (toVector smaps)

-- This could get renamed, after I look at the paper that uses the term "modulated markov"
modulatedMarkov models ratesBetween levelProbs = reversible $ markov a smap q pi where
    a = getAlphabet $ head models
    qs = map getQ models
    pis = map getEqFreqs models
    smaps = map getSMap models
    q = modulatedMarkovRates qs ratesBetween
    pi = modulatedMarkovPi pis levelProbs
    smap = modulatedMarkovSmap smaps

markovModulateMixture nu dist = modulatedMarkov models ratesBetween levelProbs where
    (models, levelProbs) = unzip $ unpackDiscrete dist
    ratesBetween = Markov.equ (length models) nu

-- We need to rescale submodels to have substitution rate `1`.
-- Otherwise class-switching rates are not relative to the substitution rate.

tuffley_steel_98_unscaled s01 s10 q = modulatedMarkov [scale 0 q, q] ratesBetween levelProbs where
    levelProbs = [s10/total, s01/total] where total = s10 + s01
    ratesBetween = fromLists [[-s01,s01],[s10,-s10]]

tuffley_steel_98 s01 s10 q = tuffley_steel_98_unscaled s01 s10 (rescale 1 q)

tuffley_steel_98_test s01 s10 fraction q = mix [1-fraction, fraction] [tuffley_steel_98_unscaled 1 0 (rescale 1 q) & unitMixture,
                                                                       tuffley_steel_98_unscaled s01 s10 (rescale 1 q) & unitMixture]

huelsenbeck_02 s01 s10 model = tuffley_steel_98_unscaled s01 s10 <$> rescale 1 model

huelsenbeck_02_test s01 s10 fraction model = mix [1-fraction, fraction] [model & huelsenbeck_02 1 0,
                                                                         -- ^ ideally we could just put "model" here.
                                                                         model & huelsenbeck_02 s01 s10]

huelsenbeck_02_two s01a s10a s01b s10b fraction model = mix [1-fraction, fraction] [model & huelsenbeck_02 s01b s10b,
                                                                                    model & huelsenbeck_02 s01a s10a]

galtier_01_ssrv nu model = modulatedMarkov models ratesBetween levelProbs where
    dist = rescale 1 model
    (models, levelProbs) = unzip $ unpackDiscrete dist
    n_levels = length models
    -- This is really a generic gtr...  We should be able to get this with f81
    ratesBetween = (Markov.equ n_levels nu) %*% (plus_f_matrix $ toVector levelProbs)

galtier_01 nu pi model = (\nu' -> galtier_01_ssrv nu' model) <$> (Discrete [(0, 1-pi), (nu, pi)])

wssr07_ssrv s01 s10 nu model = tuffley_steel_98 s01 s10 $ galtier_01_ssrv nu model

wssr07 s01 s10 nu pi model = (\nu' -> wssr07_ssrv s01 s10 nu' model) <$> (Discrete [(0, 1-pi), (nu, pi)]) 

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

-- Instead of passing ratesBetween+levelProbs, could we just pass a q matrix?
covarion_gtr_ssrv nu exchange model = modulatedMarkov models ratesBetween levelProbs where
    Discrete dist = rescale 1 model
    (models, levelProbs) = unzip dist
    -- This is really a gtr rate matrix, just without the alphabet / smap!
    ratesBetween = (scaleMatrix nu exchange) %*% (plus_f_matrix $ toVector levelProbs)

covarion_gtr nu exchange pi model = (\nu' -> covarion_gtr_ssrv nu' exchange model) <$> (Discrete [(0,1-pi), (nu, pi)])

covarion_gtr_sym :: Matrix Double -> Discrete ReversibleMarkov -> ReversibleMarkov
covarion_gtr_sym sym model = modulatedMarkov models ratesBetween levelProbs where
    dist = rescale 1 model
    (models, levelProbs) = unzip $ unpackDiscrete dist
    ratesBetween = sym %*% (plus_f_matrix $ toVector levelProbs)


