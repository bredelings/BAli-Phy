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
  let subst_root = modifiable (head $ internalNodes tree ++ leafNodes tree)

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
      fs = getNodesSet tree & IntMap.fromSet (\_ -> f)
      cls = cached_conditional_likelihoods tree nodeCLVs as transition_ps f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peel_likelihood tree nodeCLVs cls as f subst_root

      ancestralComponentStateSequences = sample_ancestral_sequences tree subst_root nodeCLVs as transition_ps f cls

      ancestral_sequences = extractStates <$> ancestralComponentStateSequences

      ancestralSequences = Aligned $ CharacterData alphabet (sequencesFromTree tree (statesToLetters smap <$> alignedSequences alignment ancestral_sequences))

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCProperties subst_root transition_ps cls ancestralSequences likelihood fs smap nodeCLVs alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel)

  return ([likelihood], prop)

instance Dist (PhyloCTMC t (AlignmentOnTree t) s) where
    type Result (PhyloCTMC t (AlignmentOnTree t) s) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (LabelType t ~ Text, HasBranchLengths t, IsTree t, SimpleSModel s) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t) s) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t) s) = PhyloCTMCProperties

    annotated_densities (PhyloCTMC tree alignment smodel scale) =
        if equilibriumReversible smodel then
            annotated_subst_like_on_tree tree alignment smodel scale
        else case isRooted tree of
               Unrooted -> error "Non-reversible or non-equilibrium substitution model requires a rooted tree!"
               Rooted -> if isStationary smodel then
                             annotatedSubstLikeOnTreeEqNonRev tree alignment smodel scale
                         else
                             annotatedSubstLikeOnTreeNonEq tree alignment smodel scale

-- getSequencesFromTree :: IsGraph t, LabelType t ~ Text => t -> IntMap Sequence ->

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


instance (IsTree t, LabelType t ~ Text, HasBranchLengths t, SimpleSModel s) => IOSampleable (PhyloCTMC t (AlignmentOnTree t) s) where
    sampleIO (PhyloCTMC tree alignment smodel scale) = do
      let alphabet = getAlphabet smodel
          smap = stateLetters smodel

      stateSequences <- case isRooted tree of
                          Unrooted -> if equilibriumReversible smodel then
                                          let tree2 = addRoot root tree where root = head $ (internalNodes tree ++ leafNodes tree)
                                          in sampleComponentStates tree2 alignment smodel scale
                                      else
                                          error "Nonreversible or non-equilibrium model requires a rooted tree!"
                          Rooted -> sampleComponentStates tree alignment smodel scale

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (IsTree t, LabelType t ~ Text, HasBranchLengths t, SimpleSModel s) => Sampleable (PhyloCTMC t (AlignmentOnTree t) s) where
    sample dist = RanDistribution2 dist do_nothing


-----------
annotatedSubstLikeOnTreeEqNonRev tree alignment smodel scale sequenceData = do
  let subst_root = modifiable (head $ internalNodes tree ++ leafNodes tree)

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
      fs = getNodesSet tree & IntMap.fromSet (\_ -> f)
      cls = cachedConditionalLikelihoodsEqNonRev tree nodeCLVs as transition_ps f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peelLikelihoodEqNonRev tree nodeCLVs cls as f subst_root

      ancestralComponentStateSequences = sample_ancestral_sequences tree subst_root nodeCLVs as transition_ps f cls

      ancestral_sequences = extractStates <$> ancestralComponentStateSequences

      ancestralSequences = Aligned $ CharacterData alphabet (sequencesFromTree tree (statesToLetters smap <$> alignedSequences alignment ancestral_sequences))

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCProperties subst_root transition_ps cls ancestralSequences likelihood fs smap nodeCLVs alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel)

  return ([likelihood], prop)

---------------------------------------------
annotatedSubstLikeOnTreeNonEq tree alignment smodel scale sequenceData = do
  let subst_root = modifiable (head $ internalNodes tree ++ leafNodes tree)

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
      cls = cachedConditionalLikelihoodsNonEq tree nodeCLVs as transition_ps f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peelLikelihoodNonEq tree nodeCLVs cls as f subst_root

      ancestralComponentStateSequences = sample_ancestral_sequences tree subst_root nodeCLVs as transition_ps f cls

      ancestral_sequences = extractStates <$> ancestralComponentStateSequences

      ancestralSequences = Aligned $ CharacterData alphabet (sequencesFromTree tree (statesToLetters smap <$> alignedSequences alignment ancestral_sequences))

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCProperties subst_root transition_ps cls ancestralSequences likelihood fs smap nodeCLVs alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel)

  return ([likelihood], prop)
