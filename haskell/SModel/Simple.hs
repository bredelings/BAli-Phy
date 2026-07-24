module SModel.Simple where

import Foreign.Vector
import Bio.Alphabet
import Tree
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import qualified Data.IntMap as IntMap (fromSet)
import Reversible

data SingleBranchLengthModel a = SingleBranchLengthModel a Double

data SModelOnTree t m = SModelOnTree t m
{-
  SModelOnTree t (SingleBranchLengthModel Markov
  SModelOnTree t ReversibleMarkov
  SModelOnTree t (Discrete Markov)
  SModelOnTree t (Discrete ReversibleMarkov)
  SModelOnTree t (Discrete (BranchModel Markov))
  SModelOnTree t (Discrete (BranchModel ReversibleMarkov))
-}

-- See LikelihoodMixtureModel

{-
 * So arguably getAlphabet + stateLetters is really part of the observation model.
 *  - arguably, because the markov chains on internal states could perhaps be describes by an alphabet as well...
 *    but just for naming purposes?

 * We could in theory allow different mixture components to have different observation models.
   - getAlphabet / stateLetters

 * It should still make sense to have weighted componentFrequencies, but the rows of the weighted frequency MATRIX
   would have different lengths.  So maybe, weighted_frequenced_vectors: m -> [Vector Double].
-}


{-
TODO: Remove stateLetters from SimpleSModel and just use getSMap directly?
TODO: Rename to e.g. PhyloLikelihood?
-}

class CheckReversible m => SimpleSModel t m where
    getTree :: (SModelOnTree t m) -> t
    stateLetters :: (SModelOnTree t m) -> EVector Int
    branchTransitionP :: (SModelOnTree t m) -> EdgeId -> [Matrix Double]
    distribution :: (SModelOnTree t m) -> [Double]
    nBaseModels :: (SModelOnTree t m) -> Int
    componentFrequencies :: (SModelOnTree t m) -> [Vector Double]

    distribution m = [1]
    nBaseModels m = length (distribution m)
    getTree (SModelOnTree tree _) = tree

-- NOTE: Component frequencies stay raw because trcall does not translate
-- EVector elements; remove this once collection-level translation exists.
foreign import trcall "SModel:weightedFrequencyMatrixRaw" weightedFrequencyMatrixNative :: Vector Double -> EVector (NativeVector Double) -> Matrix Double

-- Build weighted frequency rows while retaining model and state counts as
-- Haskell matrix dimensions.
weightedFrequencyMatrixFromVectors dist frequencies = overrideMatrixDims modelCount stateCount
    (weightedFrequencyMatrixNative dist payloads)
  where
    payloads = toVector $ map nativeVector frequencies
    modelCount = length frequencies
    stateCount = if null frequencies then 0 else vectorSize (head frequencies)

weightedFrequencyMatrix model =
    weightedFrequencyMatrixFromVectors (fromList $ distribution model)
                                       (componentFrequencies model)

frequencyMatrix model = fromRows (componentFrequencies model)

nStates m = vector_size (stateLetters m)

-- NOTE: Transition matrices stay raw because runtime vectors cannot contain
-- lifted Matrix records; remove this when the cache translates its elements.
transitionPsMap smodel_on_tree = IntMap.fromSet
    (toVector . map nativeMatrix . branchTransitionP smodel_on_tree) edges where
    edges = getEdgesSet $ getTree smodel_on_tree
