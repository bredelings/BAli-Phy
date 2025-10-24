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
import Reversible hiding (CanMakeReversible(..), reversible)    

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

rateMixture model rates = scaleBy (1/mean rates) $ rates >>= (\rate -> scaleBy rate model)

wfm (Discrete ms) = let freqs = toVector [ getStartFreqs m | (m,p) <- ms]
                        dist =  toVector [p | (m,p) <- ms ]
                    in weightedFrequencyMatrixRaw dist freqs

averageFrequency ms = vectorToList $ builtin_average_frequency $ wfm ms

plusInv :: Double -> (Discrete ReversibleMarkov) -> (Discrete ReversibleMarkov)
plusInv pInv ms = scaleBy (1/(1-pInv)) $ mix [1 - pInv, pInv] [ms, always $ inv]
    where a  = getAlphabet ms
          pi = averageFrequency ms
          inv = scaleBy 0 $ f81 pi a

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.

instance HasAlphabet m => HasAlphabet (Discrete m) where
    getAlphabet model = getAlphabet $ component model 0

instance HasSMap m => HasSMap (Discrete m) where
    getSMap model = getSMap $ component model 0

instance CheckReversible m => CheckReversible (Discrete m) where
    getReversibility (Discrete ms) = minimum [getReversibility m | (m,_) <- ms]
                    
instance (HasBranchLengths t, HasSMap m, SimpleSModel t m) => SimpleSModel t (Discrete m) where
    type instance IsReversible (Discrete m) = IsReversible m
    branchTransitionP (SModelOnTree tree model) b = concat [ branchTransitionP (SModelOnTree tree component) b | (component, _) <- unpackDiscrete model]
    distribution (SModelOnTree tree model) = concat [(pr*) <$> distribution (SModelOnTree tree component) | (component, pr) <- unpackDiscrete model]
    componentFrequencies (SModelOnTree tree model) = concat [componentFrequencies (SModelOnTree tree component) | (component,_) <- unpackDiscrete model]
    stateLetters (SModelOnTree _ model) = getSMap model

instance Scalable a => Scalable (Discrete a) where
    scaleBy x dist = fmap (scaleBy x) dist

instance RateModel a => RateModel (Discrete a) where
    rate d = mean $ fmap rate d


