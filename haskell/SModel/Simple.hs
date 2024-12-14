module SModel.Simple where

import Foreign.Vector
import Bio.Alphabet
import Tree
import Data.Matrix
import qualified Data.IntMap as IntMap (fromSet)

data SingleBranchLengthModel a = SingleBranchLengthModel a Double

data SModelOnTree t m = SModelOnTree t m Double
{-
  SModelOnTree t (SingleBranchLengthModel Markov
  SModelOnTree t ReversibleMarkov
  SModelOnTree t (Discrete Markov)
  SModelOnTree t (Discrete ReversibleMarkov)
  SModelOnTree t (MixtureModels Markov)
  SModelOnTree t (MixtureModels ReversibleMarkov)
-}

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

data EquilibriumReversible

data EquilibriumNonReversible

data NonEquilibrium

{-
TODO: Remove stateLetters from SimpleSModel and just use getSMap directly?
TODO: Rename to e.g. PhyloLikelihood?
-}

class SimpleSModel t m where
    type family IsReversible m
    type instance IsReversible m = NonEquilibrium

    getTree :: (SModelOnTree t m) -> t
    stateLetters :: (SModelOnTree t m) -> EVector Int
    branch_transition_p :: (SModelOnTree t m) -> EdgeId -> [Matrix Double]
    distribution :: (SModelOnTree t m) -> [Double]
    nBaseModels :: (SModelOnTree t m) -> Int
    componentFrequencies :: (SModelOnTree t m) -> [EVector Double]

    nBaseModels m = length (distribution m)
    getTree (SModelOnTree tree _ _) = tree

foreign import bpcall "SModel:weighted_frequency_matrix" builtin_weighted_frequency_matrix :: EVector Double -> EVector (EVector Double) -> Matrix Double
foreign import bpcall "SModel:frequency_matrix" builtin_frequency_matrix :: EVector (EVector Double) -> Matrix Double

weighted_frequency_matrix model = let dist = list_to_vector $ distribution model
                                      freqs = list_to_vector $ componentFrequencies model
                                  in builtin_weighted_frequency_matrix dist freqs

frequency_matrix model = builtin_frequency_matrix $ list_to_vector $ componentFrequencies model

nStates m = vector_size (stateLetters m)

transition_ps_map smodel_on_tree = IntMap.fromSet (list_to_vector . branch_transition_p smodel_on_tree) edges where
    edges = getEdgesSet $ getTree smodel_on_tree
