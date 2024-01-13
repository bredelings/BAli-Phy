module Probability.Distribution.PhyloCTMC.VariableA
    (module Probability.Distribution.PhyloCTMC.Properties,
     module Probability.Distribution.PhyloCTMC.PhyloCTMC
    )
where

import Probability.Distribution.PhyloCTMC.Properties
import Probability.Distribution.PhyloCTMC.PhyloCTMC
import Probability.Random
import Tree
import SModel
import SModel.Likelihood.VariableA
import Bio.Sequence -- for sequence_to_indices
import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Data.Array
import Data.Matrix
import Data.Foldable
import Foreign.Maybe
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Maybe (fromJust)

import Control.Monad.Fix -- for rec

annotated_subst_like_on_tree tree alignment smodel scale sequenceData = do
  let subst_root = modifiable (head $ internal_nodes tree ++ leaf_nodes tree)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel scale
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods tree nodeCLVs as transition_ps f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peel_likelihood tree nodeCLVs cls as f subst_root

      ancestralComponentStateSequences = sample_ancestral_sequences tree subst_root nodeCLVs as transition_ps f cls

      ancestral_sequences = extractStates <$> ancestralComponentStateSequences

      ancestralSequences = Aligned $ CharacterData alphabet (sequencesFromTree tree (statesToLetters smap <$> alignedSequences alignment ancestral_sequences))

      n_muts = parsimony tree maybeNodeSequences as alphabet (unitCostMatrix alphabet)

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = (PhyloCTMCProperties subst_root transition_ps cls ancestralSequences likelihood f smap nodeCLVs alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel) n_muts)

  return ([likelihood], prop)

instance Dist (PhyloCTMC t (AlignmentOnTree t) s Reversible) where
    type Result (PhyloCTMC t (AlignmentOnTree t) s Reversible) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (HasLabels t, HasBranchLengths t, IsTree t, SimpleSModel s) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t) s Reversible) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t) s Reversible) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotated_subst_like_on_tree tree alignment smodel scale

-- getSequencesFromTree :: HasLabels t => t -> IntMap Sequence ->

-- Uh... how do we get from a [Sequence] and an AlignmentOnTree to a FASTA file?
-- Can we have an IsAlignment class that allows us to get a FASTA from it?
--  + alignmentLength can be implemented for both AlignmentMatrix and AlignmentOnTree
--  + could we change AlignmentOnTree to be data AlignmentOnTree t = OneSequence { tree :: t, length :: Int } | ManySequences { tree :: t, paiwise_alignments :: IntMap PairwiseAlignment} ?
--    one issue is that we can check EITHER the tree OR the alignment constructor to determin of there's only one sequence.
--    We could to AlignmentOnTree { tree :: t, singleLength :: Maybe Int, pairwise_alignments :: IntMap PairwiseAlignment }
--    In that case, if there's a single length, we still have an (empty) IntMap for the pairwise alignments.

foreign import bpcall "Likelihood:" simulateRootSequence :: Int -> Matrix Double -> IO VectorPairIntInt
foreign import bpcall "Likelihood:" simulateSequenceFrom :: VectorPairIntInt -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> IO VectorPairIntInt

sampleComponentStates rtree alignment smodel scale =  do
  let as = pairwise_alignments alignment
      ps = transition_ps_map (SingleBranchLengthModel rtree smodel scale)
      f = (weighted_frequency_matrix smodel)

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence (sequenceLength alignment node) f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateSequenceFrom (stateSequences IntMap.! parent) (as IntMap.! b) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences


instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths (Rooted t), SimpleSModel s) => IOSampleable (PhyloCTMC t (AlignmentOnTree t) s Reversible) where
    sampleIO (PhyloCTMC tree alignment smodel scale) = do
      let alphabet = getAlphabet smodel
          smap = stateLetters smodel

      stateSequences <- sampleComponentStates (makeRooted tree) alignment smodel scale

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths t, HasBranchLengths (Rooted t), SimpleSModel s) => Sampleable (PhyloCTMC t (AlignmentOnTree t) s Reversible) where
    sample dist = RanDistribution2 dist do_nothing


-----------
annotatedSubstLikeOnTreeNonRev tree alignment smodel scale sequenceData = do
  let subst_root = modifiable (head $ internal_nodes tree ++ leaf_nodes tree)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel scale
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      fs = frequenciesOnTree tree f transition_ps
      cls = cachedConditionalLikelihoodsNonRev tree nodeCLVs as transition_ps fs
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peelLikelihoodNonRev tree nodeCLVs cls as fs subst_root

      ancestralComponentStateSequences = sample_ancestral_sequences tree subst_root nodeCLVs as transition_ps f cls

      ancestral_sequences = extractStates <$> ancestralComponentStateSequences

      ancestralSequences = Aligned $ CharacterData alphabet (sequencesFromTree tree (statesToLetters smap <$> alignedSequences alignment ancestral_sequences))

      n_muts = parsimony tree maybeNodeSequences as alphabet (unitCostMatrix alphabet)

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = (PhyloCTMCProperties subst_root transition_ps cls ancestralSequences likelihood f smap nodeCLVs alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel) n_muts)

  return ([likelihood], prop)

instance t ~ t2 => Dist (PhyloCTMC t (AlignmentOnTree t2) s NonReversible) where
    type Result (PhyloCTMC t (AlignmentOnTree t2) s NonReversible) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (HasLabels t, HasBranchLengths t, IsTree t, SimpleSModel s, t ~ t2) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t2) s NonReversible) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t2) s NonReversible) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotated_subst_like_on_tree tree alignment smodel scale

-- getSequencesFromTree :: HasLabels t => t -> IntMap Sequence ->

sampleComponentStatesNonRev rtree alignment smodel scale =  do
  let as = pairwise_alignments alignment
      ps = transition_ps_map (SingleBranchLengthModel rtree smodel scale)
      f = (weighted_frequency_matrix smodel)

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence (sequenceLength alignment node) f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateSequenceFrom (stateSequences IntMap.! parent) (as IntMap.! b) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences


instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths (Rooted t), SimpleSModel s) => IOSampleable (PhyloCTMC t (AlignmentOnTree t) s NonReversible) where
    sampleIO (PhyloCTMC tree alignment smodel scale) = do
      let alphabet = getAlphabet smodel
          smap = stateLetters smodel

      stateSequences <- sampleComponentStates (makeRooted tree) alignment smodel scale

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths t, HasBranchLengths (Rooted t), SimpleSModel s) => Sampleable (PhyloCTMC t (AlignmentOnTree t) s NonReversible) where
    sample dist = RanDistribution2 dist do_nothing


