module SModel.LikelihoodMixtureModel where

import Bio.Alphabet
import Data.Matrix
import Tree
import Foreign.Vector

data MixtureIndex = MixtureIndex Int

-- Could we parameterize this class by the method of determining leaf likelihoods?
-- No, that would probably be an observation model.  We might need one per mixture component.

class LikelihoodMixtureComponent m where
    -- The most general thing to do would be to specify Pr(observation=o|markov state=s) for each o and s.
    -- What we are doing here is to specify a list of states for which the probability is 1 or 0.
    -- Which could be shortened into just the list of which states are 1 - EVector (EVector Int)
    _stateToObservedState :: m -> MixtureIndex -> EVector Int
    _numObservedStates :: m -> Int
    _alphabet :: m -> Alphabet

    -- In order for this to be a general interface, the tree type needs to be hidden!
    _transitionProbabilities :: m -> Int -> Matrix Double

    _rootFrequencies :: m -> MixtureIndex -> EVector Double


class LikelihoodMixtureModel m where
    alphabet :: m -> Alphabet
    numObservedStates :: m -> Int

    componentProbabilities :: m -> [Double]
    numComponents :: m -> Int

    -- The most general thing to do would be to specify Pr(observation=o|markov state=s) for each o and s.
    -- What we are doing here is to specify a list of states for which the probability is 1 or 0.
    -- Which could be shortened into just the list of which states are 1 - EVector (EVector Int)
    stateToObservedState :: m -> MixtureIndex -> EVector Int

    numStates :: m -> MixtureIndex -> Int

    -- In order for this to be a general interface, the tree type needs to be hidden!
    transitionProbabilities :: m -> MixtureIndex -> Int -> Matrix Double

    rootFrequencies :: m -> MixtureIndex -> EVector Double

    numComponents m = length $ componentProbabilities m

-- There should be an easy enough function to compute the frequencies at each node
-- for each mixture component... we should make an Array of Arrays.
