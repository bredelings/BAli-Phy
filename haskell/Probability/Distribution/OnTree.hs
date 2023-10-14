module Probability.Distribution.OnTree where

import Probability.Random
import Tree
import SModel
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
import Probability.Distribution.List -- for independent

-- 0 - subst_root
-- 1 - transition_ps
-- 2 - CLVs
-- 3 - ?ancestral states -- used through haskell, not parameters.cc
-- 4 - likelihood
-- 5 - ??  taxa
-- 6 - weightedfrequencymatrix
-- 7 - state -> letter
-- 8 - sequences as EVector
-- 9 - alphabet
-- 10 - n_states
-- 11 - n_base_models
-- 12 - ?parsimony -- used through haskell, not parameters.cc


data CTMCOnTreeProperties = CTMCOnTreeProperties {
      prop_subst_root :: Int,
      prop_transition_ps :: IntMap (EVector (Matrix Double)),
      prop_cond_likes :: IntMap CondLikes,
      prop_anc_seqs :: Text,
      prop_likelihood :: LogDouble,
      prop_taxa :: IntMap (CMaybe CPPString),  -- Is this needed?
      prop_get_weighted_frequency_matrix :: Matrix Double,
      prop_smap :: EVector Int,
      prop_leaf_sequences :: IntMap (EVector Int),
      prop_alphabet :: Alphabet,
      prop_n_states :: Int,
      prop_n_base_models :: Int,
      prop_n_muts :: Int -- This shouldn't be here.
    }

data CTMCOnTreeFixedAProperties = CTMCOnTreeFixedAProperties {
      prop_fa_subst_root :: Int,
      prop_fa_transition_ps :: IntMap (EVector (Matrix Double)),
      prop_fa_cond_likes :: IntMap CondLikes,
      prop_fa_anc_seqs :: Text,
      prop_fa_likelihood :: LogDouble, -- Is this needed?
      prop_fa_taxa :: IntMap (CMaybe CPPString),
      prop_fa_get_weighted_frequency_matrix :: Matrix Double,
      prop_fa_smap :: EVector Int,
      prop_fa_leaf_sequences :: IntMap (EVector Int),
      prop_fa_alphabet :: Alphabet,
      prop_fa_n_states :: Int,
      prop_fa_n_base_models :: Int,
      prop_fa_n_muts :: Int -- This shouldn't be here.
    }

{- TODO:
Switch from Sequence to AlignedCharacterData / UnalignedCharacterData
-}

{- NOTE: aligned/unaligned character data object.

OK, so I'd like to have some kind of character data type (or type class) that is not in Text form.
It should be possible to print it to FASTA.

If we sample UNALIGNED character data from ctmc_on_tree, then we need to be able to combine it with an alignment to
print to sampled sequences and the alignment together.

One possibility would be something like:

  data CharacterData = CharacterData Alphabet (Map Text (EVector Int))

  instance ToFasta CharacterData

Currently, it seems like I'm using [Sequence] as a way of passing around the data.
-}

{- NOTE: Aligned and Unaligned character data

Do I want to have two different versions for aligned and unaligned character data.

-- All sequences should be the same length!
data AlignedCharacterData = Aligned CharacterData

-- No gaps!
data UnalignedCharacterData = Unaligned CharacterData

-}

{- NOTE: How should we represent ancestral states?

Currently we are representing them as Text.  Should we instead use Aligned/UnalignedCharacterData?

-}

{- NOTE: Sampling from phyloCTMC and phyloCTMCFixedA.

The idea is to sample an UnalignedCharacterData object from phyloCTMC, and an AlignedCharacterData object from phyloCTMCFixedA.

We could make a function alignCharacterData :: AlignmentOnTree -> UnalignedCharacterData -> AlignedCharacterData

For sampling from phyloCTMCFixedA, we might want two versions:
1. given a length l, sample assuming no indels or wildcards
2. given a alignment with gaps and wildcards, sample assuming no indels and then overwrite gaps and wildcards.
   PROBLEM: what about ambiguous characters?
-}

transition_ps_map smodel_on_tree = IntMap.fromSet (list_to_vector . branch_transition_p smodel_on_tree) edges where
    edges = getEdgesSet $ get_tree' smodel_on_tree

annotated_subst_like_on_tree tree alignment smodel sequences' = do
  let subst_root = modifiable (head $ internal_nodes tree ++ leaf_nodes tree)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      taxa = fmap (cMaybe . fmap Text.toCppString) $ get_labels tree
      node_sequences = fmap fromJust $ getObjectsOnTree sequences tree
      Unaligned (CharacterData _ sequences) = mkUnalignedCharacterData alphabet sequences'
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods
              tree
              node_sequences
              as
              alphabet
              transition_ps
              f
              smap
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood | n_nodes > 2    = peel_likelihood tree cls as (weighted_frequency_matrix smodel) subst_root
                 | n_nodes == 1   = let [n1] = getNodes tree
                                    in alignment `seq` peel_likelihood_1 (node_sequences IntMap.! n1) alphabet f
                 | n_nodes == 2   = let [n1, n2] = getNodes tree
                                        [b1, b2] = getEdges tree
                                    in peel_likelihood_2 (node_sequences IntMap.! n1) (node_sequences IntMap.! n2) alphabet (as IntMap.! b1) (transition_ps IntMap.! b1) f
                 | otherwise      = error $ "likelihood: n_nodes = " ++ show n_nodes

      ancestralComponentStateSequences = sample_ancestral_sequences tree subst_root node_sequences as alphabet transition_ps f cls smap

      ancestral_sequences = case n_nodes of 1 -> node_sequences
                                            2 -> node_sequences
                                            _ -> fmap extractStates ancestralComponentStateSequences

      fasta = let stateSequences = alignedSequences alignment ancestral_sequences
              in fastaTree tree (fmap (sequenceToText alphabet .  statesToLetters smap) stateSequences)

      n_muts = parsimony tree node_sequences as alphabet (unitCostMatrix alphabet)

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = (CTMCOnTreeProperties subst_root transition_ps cls fasta likelihood taxa f smap node_sequences alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel) n_muts)

  return ([likelihood], prop)

data CTMCOnTree t s = CTMCOnTree t (AlignmentOnTree t) s


instance Dist (CTMCOnTree t s) where
    type Result (CTMCOnTree t s) = [Sequence]
    dist_name _ = "ctmc_on_tree"

-- TODO: make this work on forests!                  -
instance (HasLabels t, HasBranchLengths t, IsTree t, SimpleSModel s) => HasAnnotatedPdf (CTMCOnTree t s) where
    type DistProperties (CTMCOnTree t s) = CTMCOnTreeProperties
    annotated_densities (CTMCOnTree tree alignment smodel) = annotated_subst_like_on_tree tree alignment smodel

ctmc_on_tree tree alignment smodel = CTMCOnTree tree alignment smodel

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

sampleComponentStates rtree alignment smodel =  do
  let as = pairwise_alignments alignment
      ps = transition_ps_map (SingleBranchLengthModel rtree smodel)
      f = (weighted_frequency_matrix smodel)

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence (sequenceLength alignment node) f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateSequenceFrom (stateSequences IntMap.! parent) (as IntMap.! b) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences


instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths (Rooted t), SimpleSModel s) => IOSampleable (CTMCOnTree t s) where
    sampleIO (CTMCOnTree tree alignment smodel) = do
      let alphabet = getAlphabet smodel
          smap = stateLetters smodel

      stateSequences <- sampleComponentStates (makeRooted tree) alignment smodel

      let sequenceForNode label stateSequence = (label, sequenceToText alphabet . statesToLetters smap $ extractStates stateSequence)

      return $ getLabelled tree sequenceForNode stateSequences


----------------------------------------

{-
ok, so how do we pass IntMaps to C++ functions?
well, we could turn each IntMap into an EIntMap
for alignments, we could also use an ordering of the sequences to ensure that the leaves are written first.
   -}
annotated_subst_likelihood_fixed_A tree smodel sequences = do
  let subst_root = modifiable (head $ internal_nodes tree ++ leaf_nodes tree)

  let (compressed_alignment, column_counts, mapping) = compress_alignment $ alignment_from_sequences alphabet sequences
      isequences = isequences_from_alignment compressed_alignment

      compressed_sequences = [ (name, (strip_gaps seq, bitmask_from_sequence seq)) | (name,seq) <- isequences]

      node_seqs_bits = fromMaybe (error "No label") <$> getObjectsOnTree compressed_sequences tree
      node_sequences = fmap fst node_seqs_bits
      -- stop going through Alignment

      node_sequences0 :: IntMap (Maybe (EVector Int))
      node_sequences0 = fmap (fmap $ sequenceToAlignedIndices alphabet) $ getSequencesOnTree sequences tree

      -- (compressed_node_sequences, column_counts', mapping') = compress_sequences node_sequence0
      -- OK, so how are we going to do this?
      -- * turn the sequences into an EIntMap (EVector Int)
      -- * run the pattern compression on the EIntMap (EVector Int)
      -- * return the compressed EIntMap (EVector Int), an EVector Int for the orig->compress mapping, and an EVector Int for the column counts).
      -- * convert the EIntMap (EVector Int) back to an IntMap EVector Int for the compressed sequences.

      -- OK, so now we need to get the ancestral sequences as an IntMap (EVector Int)
      -- So... actually it looks like we already HAVE a minimally_connect_leaf_characters function!!!

      n_nodes = numNodes tree
      taxa = fmap (cMaybe . fmap Text.toCppString) $ get_labels tree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods_SEV
              tree
              node_seqs_bits
              alphabet
              transition_ps
              smap
      likelihood | n_nodes > 2    = peel_likelihood_SEV tree cls f subst_root column_counts
                 | n_nodes == 1   = peel_likelihood_1_SEV compressed_alignment alphabet f column_counts
                 | n_nodes == 2   = let [n1,n2] = getNodes tree
                                        [b1,b2] = getEdges tree
                                    in peel_likelihood_2_SEV compressed_alignment alphabet (transition_ps IntMap.! b1) f column_counts

--    This also needs the map from columns to compressed columns:
      ancestral_sequences = case n_nodes of
                              1 -> Text.concat [fastaSeq s | s <- sequences]
                              2 -> Text.concat [fastaSeq s | s <- sequences]
                              _ -> let ancestralComponentStateSequences :: IntMap VectorPairIntInt
                                       ancestralComponentStateSequences = sample_ancestral_sequences_SEV
                                         tree
                                         subst_root
                                         node_sequences
                                         alphabet
                                         transition_ps
                                         f
                                         cls
                                         smap
                                         mapping
                                       ancestralStateSequences :: IntMap (EVector Int)
                                       ancestralStateSequences = fmap extractStates ancestralComponentStateSequences
                                       ancestralStateSequences' = minimally_connect_characters
                                                                        node_sequences0
                                                                        tree
                                                                        ancestralStateSequences
                                       ancestralLetterSequences = fmap (sequenceToText alphabet . statesToLetters smap) ancestralStateSequences'
                                   in fastaTree tree ancestralLetterSequences

      n_muts = parsimony_fixed_A tree node_seqs_bits alphabet (unitCostMatrix alphabet) column_counts

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  let prop = (CTMCOnTreeFixedAProperties subst_root transition_ps cls ancestral_sequences likelihood taxa f smap node_sequences alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel) n_muts)

  return ([likelihood], prop)

data CTMCOnTreeFixedA t s = CTMCOnTreeFixedA t s

instance Dist (CTMCOnTreeFixedA t s) where
    type Result (CTMCOnTreeFixedA t s) = [Sequence]
    dist_name _ = "ctmc_on_tree_fixed_A"

-- TODO: make this work on forests!                  -
instance (HasLabels t, HasBranchLengths t, IsTree t, SimpleSModel s) => HasAnnotatedPdf (CTMCOnTreeFixedA t s) where
    type DistProperties (CTMCOnTreeFixedA t s) = CTMCOnTreeFixedAProperties
    annotated_densities (CTMCOnTreeFixedA tree smodel) = annotated_subst_likelihood_fixed_A tree smodel

ctmc_on_tree_fixed_A tree smodel = CTMCOnTreeFixedA tree smodel
