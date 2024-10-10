module SModel.Simple where

import Foreign.Vector
import Bio.Alphabet
import Tree
import Data.Matrix
import qualified Data.IntMap as IntMap (fromSet)
import Markov (CheckReversible(..))

data SingleBranchLengthModel t a = SingleBranchLengthModel t a Double
get_tree' (SingleBranchLengthModel t _ _) = t        -- Avoid aliasing with get_tree from DataPartition

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

class (CheckReversible m, HasAlphabet m) => SimpleSModel m where
    stateLetters :: m -> EVector Int
    branch_transition_p :: HasBranchLengths t => SingleBranchLengthModel t m -> Int -> [Matrix Double]
    distribution :: m -> [Double]
    nBaseModels :: m -> Int
    componentFrequencies :: m -> Int -> EVector Double

foreign import bpcall "SModel:weighted_frequency_matrix" builtin_weighted_frequency_matrix :: EVector Double -> EVector (EVector Double) -> Matrix Double
foreign import bpcall "SModel:frequency_matrix" builtin_frequency_matrix :: EVector (EVector Double) -> Matrix Double

weighted_frequency_matrix model = let dist = list_to_vector $ distribution model
                                      freqs = list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]
                                  in builtin_weighted_frequency_matrix dist freqs

frequency_matrix model = builtin_frequency_matrix $ list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]

nStates m = vector_size (stateLetters m)

transition_ps_map smodel_on_tree = IntMap.fromSet (list_to_vector . branch_transition_p smodel_on_tree) edges where
    edges = getEdgesSet $ get_tree' smodel_on_tree
