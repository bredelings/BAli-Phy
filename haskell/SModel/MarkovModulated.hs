module SModel.MarkovModulated where

import Foreign.Vector
import Bio.Alphabet
import Reversible
import SModel.ReversibleMarkov
import SModel.MixtureModel
import SModel.Property
import SModel.Rate
import qualified Markov
import Markov (getQ, getStartFreqs, getEqFreqs)
import Numeric.LinearAlgebra -- for fromLists and scale
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric.LinearAlgebra.Data

-- NOTE: Component matrices stay raw because trcall does not translate EVector
-- elements; remove this once collection-level translation exists.
foreign import trcall "SModel:modulated_markov_rates" modulatedRatesNative
    :: EVector (NativeMatrix Double) -> Matrix Double -> EVector (NativeVector Double) -> Matrix Double
-- NOTE: Component frequencies stay raw because trcall does not translate
-- EVector elements; remove this once collection-level translation exists.
foreign import trcall "SModel:modulated_markov_pi" modulatedPiNative :: EVector (NativeVector Double) -> Vector Double -> Vector Double

-- AlwaysSwitch leaves proposed switching rates unchanged; MetropolisSwitching adjusts them
-- using component equilibrium frequencies, independently of their starting distributions.
data SwitchingMethod = AlwaysSwitch | MetropolisSwitching deriving (Show)

-- An empty native frequency vector selects direct switching without evaluating equilibria.
-- Otherwise the native builder applies the pairwise Metropolis factor at each switching edge.
modulatedMarkovRates method qs pis rates_between = overrideMatrixDims stateCount stateCount
    (modulatedRatesNative (toVector (map nativeMatrix qs)) rates_between frequencies)
  where
    stateCount = sum (map rows qs)
    frequencies = toVector $ case method of
        AlwaysSwitch -> []
        MetropolisSwitching -> map nativeVector pis

modulatedMarkovPi pis levelProbs = overrideVectorSize stateCount
    (modulatedPiNative (toVector (map nativeVector pis)) levelProbs)
  where stateCount = sum (map vectorSize pis)

modulatedMarkov method models between = modulatedMarkovNamed method (map show [1..length models]) models between

-- Match the native component-major order, retaining each component's state names under a unique prefix.
-- The prefixes supplied here are regime indices, or OFF/ON for the Tuffley-Steel construction.
modulatedMarkovNamed method prefixes models between
    | null models = error "modulatedMarkov: no component models"
    | not (all (== head smaps) (tail smaps)) =
        error "modulatedMarkov: components must have the same state-to-letter mapping"
    | otherwise = setModulatedConditions conditions $ setModulatedProperties properties baseModel
  where
    a = getAlphabet $ head models
    qs = map getQ models
    pis = map getStartFreqs models
    smaps = map getSMap models
    q = modulatedMarkovRates method qs (map getEqFreqs models) (getQ between)
    pi = modulatedMarkovPi pis (getStartFreqs between)
    smap = U.concat smaps
    names = V.fromList [prefix ++ ":" ++ name | (prefix, model) <- zip prefixes models, name <- stateNames model]
    -- Equal component equilibria preserve the product for direct switching. Metropolis also
    -- preserves it for unequal equilibria when switching is EqRev. NonEq roots remain NonEq.
    rev | componentRev == NonEq || switchingRev == NonEq = NonEq
        | otherwise = case method of
            MetropolisSwitching | switchingRev == EqRev -> componentRev
            _ | sameFrequencies -> min componentRev switchingRev
              | otherwise -> NonEq
    componentRev = minimum $ map getReversibility models
    switchingRev = getReversibility between
    sameFrequencies = all (vectorEqual (head pis)) (tail pis)
    baseModel = setReversibility rev $ markov a smap names q pi
    properties = modulateCommonProperties $ fmap getStatePropertyFunctions models
    conditions = commonOrConditionMap $ fmap getComponentConditions models

-- Install common input properties after flattening each component/state
-- property vector into the output modulated Markov state order.
setModulatedProperties propertyMap model = foldl addProperty model (Map.toAscList propertyMap)
    where addProperty m (name, property) = setStateProperty name property m

-- Install the OR-reduced component conditions on the single expanded Markov component.
setModulatedConditions conditionMap model = foldl addCondition model (Map.toAscList conditionMap)
    where addCondition m (name, value) = setComponentCondition name value m

-- Retain only properties supplied by every component model and pass later
-- scaling through to each component property before flattening.
modulateCommonProperties [] = Map.empty
modulateCommonProperties (properties:rest) = Map.fromList
    [ (name, modulateProperties (property:[p Map.! name | p <- rest]))
    | (name, property) <- Map.toAscList properties
    , all (Map.member name) rest
    ]

modulateProperties properties scale = markovModulateProperty $ ComponentStateProperties [property scale | property <- properties]

markovModulateMixture method nu dist = modulatedMarkov method models (Markov.gtr ratesBetween levelProbs) where
    (models, levelProbs) = unzip $ unpackDiscrete dist
    ratesBetween = Markov.equ (length models) nu

-- We need to scaleTo submodels to have substitution rate `1`.
-- Otherwise class-switching rates are not relative to the substitution rate.

tuffleySteel98Unscaled s01 s10 q =
    modulatedMarkovNamed AlwaysSwitch ["OFF", "ON"] [scaleBy 0 q, q]
        (setReversibility EqRev $ Markov.markov ratesBetween (fromList levelProbs))
  where
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

galtier01Ssrv method nu model = modulatedMarkov method models (Markov.gtr (Markov.equ nLevels nu) levelProbs) where
    dist = scaleTo 1 model
    (models, levelProbs) = unzip $ unpackDiscrete dist
    nLevels = length models

galtier01 method nu pi model = (\nu' -> galtier01Ssrv method nu' model) <$> (Discrete [(0, 1-pi), (nu, pi)])

wssr07Ssrv method s01 s10 nu model = tuffleySteel98 s01 s10 $ galtier01Ssrv method nu model

-- Wang et al. (2007), Fig. 1C: noncovarion sites never switch rates, including ON/OFF.
-- Their mean rate is 1; the covarion component's mean is s01/(s01+s10) before overall scaling.
wssr07 method s01 s10 nu pi model = Discrete [(noCov, 1-pi), (cov, pi)]
    where noCov = wssr07Ssrv method 1 0 0 model
          cov = wssr07Ssrv method s01 s10 nu model

-- a -> HB02
-- b -> GT01 if no HB02
-- c -> GT01 if    HB02

-- X = (1-a)(1-b)
-- X + HB02 = a(1-c)
-- X + GT01 = (1-a)b
-- X + HB02+GT01 = a * c

wssr07Ext method s01 s10 nu a b c model = Discrete [(noCov,   (1-a)*(1-b)),
                                             (gt01,    (1-a)*b),
                                             (hb02,        a*(1-c)),
                                             (hb02gt01,    a*c)]
    where noCov = wssr07Ssrv method 1 0 0  model
          gt01  = wssr07Ssrv method 1 0 nu model
          hb02  = wssr07Ssrv method s01 s10 0 model
          hb02gt01 = wssr07Ssrv method s01 s10 nu model

-- Instead of passing ratesBetween+levelProbs, could we just pass a q matrix?
covarionGtrSsrv method nu exchange model =
    modulatedMarkov method models (setReversibility EqRev $ Markov.markov ratesBetween (fromList levelProbs))
  where
    Discrete dist = scaleTo 1 model
    (models, levelProbs) = unzip dist
    -- This is really a gtr rate matrix, just without the alphabet / smap!
    ratesBetween = (scale nu exchange) * (plus_f_matrix $ fromList levelProbs)

covarionGtr method nu exchange pi model = (\nu' -> covarionGtrSsrv method nu' exchange model) <$> (Discrete [(0,1-pi), (nu, pi)])

covarionGtrSym method sym model =
    modulatedMarkov method models (setReversibility EqRev $ Markov.markov ratesBetween (fromList levelProbs))
  where
    dist = scaleTo 1 model
    (models, levelProbs) = unzip $ unpackDiscrete dist
    ratesBetween = sym * (plus_f_matrix $ fromList levelProbs)
