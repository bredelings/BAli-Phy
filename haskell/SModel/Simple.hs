module SModel.Simple where

import Foreign.Vector
import Bio.Alphabet
import Tree
import Data.Matrix
import qualified Data.IntMap as IntMap (fromSet)

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

class HasAlphabet m => SimpleSModel m where
    stateLetters :: m -> EVector Int

    branch_transition_p :: HasBranchLengths t => SingleBranchLengthModel t m -> Int -> [Matrix Double]
    distribution :: m -> [Double]
    weighted_frequency_matrix :: m -> Matrix Double
    frequency_matrix :: m -> Matrix Double
    nBaseModels :: m -> Int
    componentFrequencies :: m -> Int -> EVector Double

nStates m = vector_size (stateLetters m)

transition_ps_map smodel_on_tree = IntMap.fromSet (list_to_vector . branch_transition_p smodel_on_tree) edges where
    edges = getEdgesSet $ get_tree' smodel_on_tree

transition_ps_map2 smodel_on_tree = getEdgesSet tree & IntMap.fromSet branchToPs
    where tree = get_tree' smodel_on_tree
          branchToPs b = list_to_vector $ maybeTr b $ branch_transition_p smodel_on_tree b
          maybeTr b = if not $ toward_root tree b then map tr else id

