module SModel.MarkovModulated where

import Foreign.Vector
import Bio.Alphabet
import Reversible hiding (reversible)
import SModel.ReversibleMarkov
import SModel.MixtureModel
import SModel.Rate
import qualified Markov
import Markov (getQ, getStartFreqs, getEqFreqs)
import Data.Matrix -- for fromLists, %*%

foreign import bpcall "SModel:modulated_markov_rates" builtin_modulated_markov_rates :: EVector (Matrix Double) -> Matrix Double -> Matrix Double
foreign import bpcall "SModel:modulated_markov_pi" builtin_modulated_markov_pi :: EVector (EVector Double) -> EVector Double -> EVector Double
foreign import bpcall "SModel:modulated_markov_smap" builtin_modulated_markov_smap :: EVector (EVector Int) -> EVector Int

modulatedMarkovRates qs rates_between = builtin_modulated_markov_rates (toVector qs) rates_between

modulatedMarkovPi pis levelProbs = builtin_modulated_markov_pi (toVector pis) (toVector levelProbs)

modulatedMarkovSmap smaps = builtin_modulated_markov_smap (toVector smaps)

{- NOTE: Does ratesBetween + levelProbs = GTR ?

   If so, I could pass:
   - Qs = a list of n rate matrices
   - S  = a rate matrix on n states
   Having the matrix S=GTR(ratesBetween,levelProbs) would be a special case.

   If all the Qs and the S are reversible then the whole thing should be reversible.
   QUESTION: How would we record this?
 -}

modulatedMarkov models between = reversible $ setReversibility rev $ markov a smap q pi where
    a = getAlphabet $ head models
    qs = map getQ models
    pis = map getStartFreqs models
    smaps = map getSMap models
    q = modulatedMarkovRates qs (getQ between)
    pi = modulatedMarkovPi pis (getStartFreqs between)
    smap = modulatedMarkovSmap smaps
    rev = (minimum $ fmap getReversibility models) `min` (getReversibility between)

markovModulateMixture nu dist = modulatedMarkov models (Markov.gtr ratesBetween (toVector levelProbs)) where
    (models, levelProbs) = unzip $ unpackDiscrete dist
    ratesBetween = Markov.equ (length models) nu

-- We need to scaleTo submodels to have substitution rate `1`.
-- Otherwise class-switching rates are not relative to the substitution rate.

tuffleySteel98Unscaled s01 s10 q = modulatedMarkov [scaleBy 0 q, q] (Markov.markov ratesBetween (toVector levelProbs)) where
    levelProbs = [s10/total, s01/total] where total = s10 + s01
    ratesBetween = fromLists [[-s01,s01],[s10,-s10]]

tuffleySteel98 s01 s10 q = tuffleySteel98Unscaled s01 s10 (scaleTo 1 q)

tuffleySteel98Test s01 s10 fraction q = mix [1-fraction, fraction] [tuffleySteel98 1   0   q & unitMixture,
                                                                    tuffleySteel98 s01 s10 q & unitMixture]

huelsenbeck02 s01 s10 model = tuffleySteel98Unscaled s01 s10 <$> scaleTo 1 model

huelsenbeck02Test s01 s10 fraction model = mix [1-fraction, fraction] [model & huelsenbeck02 1 0,
                                                                       model & huelsenbeck02 s01 s10]

huelsenbeck02Two s01a s10a s01b s10b fraction model = mix [1-fraction, fraction] [model & huelsenbeck02 s01b s10b,
                                                                                  model & huelsenbeck02 s01a s10a]

galtier01Ssrv nu model = modulatedMarkov models (Markov.markov ratesBetween (toVector levelProbs)) where
    dist = scaleTo 1 model
    (models, levelProbs) = unzip $ unpackDiscrete dist
    nLevels = length models
    -- This is really a generic gtr...  We should be able to get this with f81
    ratesBetween = (Markov.equ nLevels nu) %*% (plus_f_matrix $ toVector levelProbs)

galtier01 nu pi model = (\nu' -> galtier01Ssrv nu' model) <$> (Discrete [(0, 1-pi), (nu, pi)])

wssr07Ssrv s01 s10 nu model = tuffleySteel98 s01 s10 $ galtier01Ssrv nu model

wssr07 s01 s10 nu pi model = (\nu' -> wssr07Ssrv s01 s10 nu' model) <$> (Discrete [(0, 1-pi), (nu, pi)]) 

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
    where noCov = wssr07Ssrv 1 0 0  model
          gt01  = wssr07Ssrv 1 0 nu model
          hb02  = wssr07Ssrv s01 s10 0 model
          hb02gt01 = wssr07Ssrv s01 s10 nu model

-- Instead of passing ratesBetween+levelProbs, could we just pass a q matrix?
covarionGtrSsrv nu exchange model = modulatedMarkov models (Markov.markov ratesBetween (toVector levelProbs)) where
    Discrete dist = scaleTo 1 model
    (models, levelProbs) = unzip dist
    -- This is really a gtr rate matrix, just without the alphabet / smap!
    ratesBetween = (scaleMatrix nu exchange) %*% (plus_f_matrix $ toVector levelProbs)

covarionGtr nu exchange pi model = (\nu' -> covarionGtrSsrv nu' exchange model) <$> (Discrete [(0,1-pi), (nu, pi)])

covarionGtrSym :: Matrix Double -> Discrete ReversibleMarkov -> ReversibleMarkov
covarionGtrSym sym model = modulatedMarkov models (Markov.markov ratesBetween (toVector levelProbs)) where
    dist = scaleTo 1 model
    (models, levelProbs) = unzip $ unpackDiscrete dist
    ratesBetween = sym %*% (plus_f_matrix $ toVector levelProbs)


