module SModel.Simple where

import Foreign.Vector
import Bio.Alphabet
import Tree
import Data.Matrix
import qualified Data.IntMap as IntMap (fromSet)
import Reversible

data SingleBranchLengthModel a = SingleBranchLengthModel a Double

data SModelOnTree t m = SModelOnTree t m
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

class CheckReversible m => SimpleSModel t m where
    type family IsReversible m
    type instance IsReversible m = NonEquilibrium

    getTree :: (SModelOnTree t m) -> t
    stateLetters :: (SModelOnTree t m) -> EVector Int
    branchTransitionP :: (SModelOnTree t m) -> EdgeId -> [Matrix Double]
    distribution :: (SModelOnTree t m) -> [Double]
    nBaseModels :: (SModelOnTree t m) -> Int
    componentFrequencies :: (SModelOnTree t m) -> [EVector Double]

    distribution m = [1]
    nBaseModels m = length (distribution m)
    getTree (SModelOnTree tree _) = tree

foreign import bpcall "SModel:" weightedFrequencyMatrixRaw :: EVector Double -> EVector (EVector Double) -> Matrix Double
foreign import bpcall "SModel:" frequencyMatrixRaw :: EVector (EVector Double) -> Matrix Double

weightedFrequencyMatrix model = let dist = toVector $ distribution model
                                    freqs = toVector $ componentFrequencies model
                                in weightedFrequencyMatrixRaw dist freqs

frequencyMatrix model = frequencyMatrixRaw $ toVector $ componentFrequencies model

nStates m = vector_size (stateLetters m)

transitionPsMap smodel_on_tree = IntMap.fromSet (toVector . branchTransitionP smodel_on_tree) edges where
    edges = getEdgesSet $ getTree smodel_on_tree
