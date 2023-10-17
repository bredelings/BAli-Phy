module Bio.Alignment (module Bio.Alignment,
                      module Bio.Sequence,
                      module Bio.Alignment.Matrix,
                      module Bio.Alignment.Pairwise) where

import Tree
import Data.BitVector
import Data.Foldable
import Data.Array
import Data.Maybe (catMaybes)
import Parameters
import Foreign.Vector
import Foreign.Pair
import Bio.Sequence
import Bio.Alphabet -- for Alphabet type
import Bio.Alignment.Matrix
import Bio.Alignment.Pairwise

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Data.Text as Text
import Data.Text (Text)

import Foreign.IntMap (EIntMap)
import qualified Foreign.IntMap as FIM

import qualified Data.Map as Map
import Data.Map (Map)

data VectorPairIntInt -- ancestral sequences with (int letter, int category) for each site.

foreign import bpcall "Alignment:leaf_sequence_counts" builtin_leaf_sequence_counts :: AlignmentMatrix -> Int -> EVector Int -> EVector (EVector Int)

branch_hmms (model,_) tree = getUEdgesSet tree & IntMap.fromSet (model $ branch_lengths tree)
  
seqlength as tree node = pairwise_alignment_length1 (as IntMap.! b) where
    b = head $ edgesOutOfNode tree node

{-
pairwise_alignments_from_matrix a tree = [ pairwise_alignment_from_bits bits1 bits2 | b <- [0..2*numBranches tree-1],
                                                                                           let bits1 = bits ! sourceNode tree b,
                                                                                           let bits2 = bits ! targetNode tree b]
    where bits = minimally_connect_characters a tree
-}

{- NOTE: How should we handle forests?
Currently we store the node sequence lengths directory only for nodes with observed data.
We could alternatively store the node sequence lengths only for nodes not in a pairwise alignment.
-}

-- We can't just do forall t.AlignmentOnTree t, because then any constraints on t will be on existential variables, resulting in ambiguity.
data AlignmentOnTree t = AlignmentOnTree t Int (IntMap Int) (IntMap PairwiseAlignment)

n_sequences         (AlignmentOnTree _ n _  _) = n

pairwise_alignments (AlignmentOnTree _ _ _ as) = as

sequenceLength (AlignmentOnTree t n ls as) node =
    case IntMap.lookup node ls of Just length -> length
                                  Nothing -> case edgesOutOfNode t node of (b:_) -> pairwise_alignment_length1 (as IntMap.! b)
                                                                           _ -> error "sequenceLength: no length for degree-0 node!"


mkSequenceLengthsMap a@(AlignmentOnTree tree _ _ _) = getNodesSet tree & IntMap.fromSet (\node -> sequenceLength a node)

type EAlignment = EVector (EPair CPPString (EVector Int))
toEAlignment a = list_to_vector [ c_pair (Text.toCppString label) indices | (label, indices) <- a ]
fromEAlignment ea = map ( (\(x,y) -> (Text.fromCppString x,y)) . pair_from_c) $ list_from_vector ea

-- Current a' is an alignment, but counts and mapping are EVector
-- AlignmentMatrix -> ETuple (AlignmentMatrix, EVector Int, EVector Int)
foreign import bpcall "Alignment:compress_alignment" builtin_compress_alignment :: EAlignment ->
                                                                                   EPair EAlignment (EPair (EVector Int) (EVector Int))
compress_alignment a = (compressed, counts, mapping) where tmp123 = builtin_compress_alignment (toEAlignment a)
                                                           (compressed', tmp23) = pair_from_c tmp123
                                                           (counts, mapping) = pair_from_c tmp23
                                                           compressed = fromEAlignment compressed'

alignment_on_tree_length a@(AlignmentOnTree t _ _ as) = (sequenceLength a node0) + sum [numInsert (as IntMap.! b) | b <- allEdgesFromNode t node0]
                                                       where node0 = head $ getNodes t

totalNumIndels (AlignmentOnTree t _ _ as) = sum [numIndels (as IntMap.! b) | b <- allEdgesFromNode t node0]
                                         where node0 = head $ getNodes t

totalLengthIndels (AlignmentOnTree t _ _ as) = sum [lengthIndels (as IntMap.! b) | b <- allEdgesFromNode t node0]
                                         where node0 = head $ getNodes t

-- Alignment -> Int -> EVector Int -> [EVector Int]
leaf_sequence_counts a n counts = list_from_vector $ builtin_leaf_sequence_counts a n counts

foreign import bpcall "Alignment:ancestral_sequence_alignment" builtin_ancestral_sequence_alignment :: AlignmentMatrix -> EVector VectorPairIntInt -> EVector Int -> AlignmentMatrix
ancestral_sequence_alignment tree a0 states smap = builtin_ancestral_sequence_alignment a0 states' smap
    where states' = list_to_vector [ states IntMap.! node  | node <- sort $ getNodes tree]

-- Extract (component,state) -> state.
foreign import bpcall "Alignment:" extractStates :: VectorPairIntInt -> EVector Int


{-
  OK, so what do we use the original alignment matrix for?
  * the alphabet
  * the original sequences.

  Right now, I think the node names still correspond to the input alignment names.
  But when the node names stop being [0..n_sequences-1], we will need a way to
  1. construct the alignment from a list of Sequence objects
  2. get a list (leaf_nodes ++ internal_nodes)
  3. for each node, get the leaf label
  4. use smap to project the state to an alphabet letter.
  5. turn the list of Sequence objects into an alignment.
Maybe we want a map from String -> EVector Int.

That would lose the comments on the fastas though.
-}

foreign import bpcall "Alignment:select_alignment_columns" builtin_select_alignment_columns :: AlignmentMatrix -> EVector Int -> AlignmentMatrix
select_alignment_columns alignment sites = builtin_select_alignment_columns alignment (list_to_vector sites)

foreign import bpcall "Alignment:select_alignment_pairs" builtin_select_alignment_pairs :: AlignmentMatrix -> EVector (EPair Int Int) -> Alphabet -> AlignmentMatrix
select_alignment_pairs alignment sites doublets = builtin_select_alignment_pairs alignment sites' doublets
    where sites' = list_to_vector $ map (\(x,y) -> c_pair x y) sites

alignmentOnTreeFromSequences tree (Aligned sequences) = AlignmentOnTree tree numSequences lengths pairwiseAs
    where -- observedSequences :: IntMap (Maybe (EVector Int))
          observedSequences = labelToNodeMap tree $ getSequences sequences
          allSequences = minimally_connect_characters' observedSequences tree
          numSequences = length $ getSequences sequences
          lengths = getNodesSet tree & IntMap.fromSet (\node -> vector_size $ strip_gaps $ allSequences IntMap.! node)
          bits = fmap bitmask_from_sequence' allSequences
          alignmentForBranch b = pairwise_alignment_from_bits (bits IntMap.! source) (bits IntMap.! target)
                                 where source = sourceNode tree b
                                       target = targetNode tree b
          pairwiseAs = getEdgesSet tree & IntMap.fromSet alignmentForBranch


find_sequence label sequences = find (\s -> fst s == label) sequences

-- Get a map from labeled nodes to Just v, and unlabeled nodes to Nothing.
-- Complain if a labeled node doesn't have a corresponding entry in the Map.
labelToNodeMap :: HasLabels t => t -> [(Text, v)] -> IntMap (Maybe v)
labelToNodeMap tree things = getNodesSet tree & IntMap.fromSet objectForNode where
    objectForNode node = case get_label tree node of
                           Nothing -> Nothing
                           Just label ->
                               case lookup label things of
                                 Just object -> Just object
                                 Nothing -> error $ "No object for node labeled " ++ Text.unpack label

getSequencesOnTree :: HasLabels t => [Sequence] -> t -> IntMap (Maybe Sequence)
getSequencesOnTree sequence_data tree = getNodesSet tree & IntMap.fromSet sequence_for_node where
    sequence_for_node node = case get_label tree node of
                               Nothing ->  Nothing
                               Just label ->
                                   case find_sequence label sequence_data of
                                     Just sequence -> Just sequence
                                     Nothing -> error $ "No such sequence " ++ Text.unpack label

foreign import bpcall "Vector:showObject" showVectorPairIntInt :: VectorPairIntInt -> CPPString
instance Show VectorPairIntInt where
    show = unpack_cpp_string . showVectorPairIntInt


getLabelled :: HasLabels t => t -> (Text -> a -> b) -> IntMap a -> [b]
getLabelled tree f things = catMaybes $ fmap go $ IntMap.toList things where
    go (node, thing) = case get_label tree node of
                         Just label -> Just $ f label thing
                         Nothing -> Nothing

fastaTree tree sequences =  fastaSeqs [(label, sequence) | n <- leaf_nodes tree ++ internal_nodes tree,
                                                             let label = add_ancestral_label n (get_labels tree),
                                                             let sequence = sequences IntMap.! n]

-- Ideally we'd like to do
--    type NodeAlignment = EPair Int BranchAlignments
-- but this creates recursive type synonyms, which are not allowed.

-- We hack around this by removing the definition of NodeAlignment.
-- Then NodeAlignment needs a foreign import to construct it.

-- This constructs an all-insert alignment.
data NodeAlignment -- NodeAlignment SourceNode Int (EVector BranchAlignment)
foreign import bpcall "Alignment:" mkNodeAlignment :: Int -> Int -> EVector BranchAlignment -> NodeAlignment

data BranchAlignment -- BranchAlignment TargetNode PairwiseAlignment (EVector BranchAlignment)
foreign import bpcall "Alignment:" mkBranchAlignment :: Int -> PairwiseAlignment -> EVector BranchAlignment -> BranchAlignment

exportAlignmentOnTree :: IsTree t => AlignmentOnTree t -> NodeAlignment
exportAlignmentOnTree a@(AlignmentOnTree tree _ _ as) = mkNodeAlignment root (sequenceLength a root) (branchAlignments $ edgesOutOfNodeArray tree root)
    where root = head $ getNodes tree
          branchAlignments edges = list_to_vector [ mkBranchAlignment (targetNode tree e) (as IntMap.! e) (branchAlignments $ edgesAfterEdgeArray tree e) | e <- toList $ edges]

foreign import bpcall "Alignment:" substituteLetters :: EVector Int -> EVector Int -> EVector Int

foreign import bpcall "Alignment:" constructPositionSequencesRaw :: NodeAlignment -> EIntMap (EVector Int)
constructPositionSequences a = FIM.importIntMap $ constructPositionSequencesRaw $ exportAlignmentOnTree a

alignedSequences alignment@(AlignmentOnTree tree _ _ _) sequences = getNodesSet tree & IntMap.fromSet stateSequenceForNode
    where positionSequences = constructPositionSequences alignment
          stateSequenceForNode n = substituteLetters (sequences IntMap.! n) (positionSequences IntMap.! n)
