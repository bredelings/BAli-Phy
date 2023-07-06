module SModel.Simple where

import Foreign.Vector
import Bio.Alphabet
import Tree

data SingleBranchLengthModel t a = SingleBranchLengthModel t a
get_tree' (SingleBranchLengthModel t _) = t        -- Avoid aliasing with get_tree from DataPartition

-- See LikelihoodMixtureModel

{-
 * So arguably getAlphabet + stateLetters is really part of the observation model.
 *  - arguably, because the markov chains on internal states could perhaps be describes by an alphabet as well...
 *    but just for naming purposes?

 * We could in theory allow different mixture components to have different observation models.
   - getAlphabet / stateLetters

 * It should still make sense to have weighted componentFrequencies, but the rows of the weighted frequency MATRIX
   would have different lengths.  So maybe, weighted_frequenced_vectors: m -> EVector EVector Double.
-}

class SimpleSModel m where
    stateLetters :: m -> EVector Int
    getAlphabet :: m -> Alphabet

    branch_transition_p :: HasBranchLengths t => SingleBranchLengthModel t m -> Int -> [Matrix Double]
    distribution :: m -> [Double]
    weighted_frequency_matrix :: m -> Matrix Double
    frequency_matrix :: m -> Matrix Double
    nBaseModels :: m -> Int
    componentFrequencies :: m -> Int -> EVector Double

nStates m = vector_size (stateLetters m)
