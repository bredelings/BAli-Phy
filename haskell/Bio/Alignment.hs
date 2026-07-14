{-# LANGUAGE TypeFamilies #-}
module Bio.Alignment (module Bio.Alignment,
                      module Bio.Sequence,
                      module Bio.Alignment.Matrix,
                      module Bio.Alignment.Pairwise,
                      module Bio.Alignment.Class) where

import Tree
import Compiler.FFI.Import (CInput(..))
import Compiler.FFI.Runtime (RuntimeValue)
import Data.BitVector
import Data.Foldable
import Data.Maybe (catMaybes, fromMaybe)
import Parameters
import Foreign.Vector
import Foreign.Pair
import Bio.Sequence
import Bio.Alphabet -- for Alphabet type
import Bio.Alignment.Matrix
import Bio.Alignment.Pairwise
import Bio.Alignment.Class

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (doubleVectorFromNativeWithLength,
                                     intVectorFromNative,
                                     intVectorFromNativeWithLength,
                                     intVectorNativeView)
import Foreign.NativeVector (NativeVector)

import qualified Data.Text as Text
import Data.Text (Text)

import Foreign.IntMap (EIntMap)
import qualified Foreign.IntMap as FIM

import qualified Data.Map as Map

import Data.JSON

import qualified Data.JSON.Encoding as E

-- Store the sampled substitution component and state for each site in two
-- unboxed primitive arrays while retaining their domain-specific JSON rules.
data ComponentStateSequence = ComponentStateSequence (U.Vector (Int,Int))

type NativeComponentStateSequence = EPair (NativeVector Int) (NativeVector Int)

componentStateSequenceFromNative :: Int -> NativeComponentStateSequence -> ComponentStateSequence
componentStateSequenceFromNative count result =
    ComponentStateSequence
        (U.zip (intVectorFromNativeWithLength count components)
               (intVectorFromNativeWithLength count states))
  where
    (components, states) = pair_from_c result

-- Return both complete native child views so foreign consumers preserve
-- independently sliced component and state arrays.
componentStateSequenceNativeView :: ComponentStateSequence ->
    (Int, Int, NativeVector Int, Int, NativeVector Int)
componentStateSequenceNativeView (ComponentStateSequence values) =
    (U.length values, componentOffset, componentNative,
                      stateOffset, stateNative)
  where
    (components, states) = U.unzip values
    (componentOffset, _, componentNative) = intVectorNativeView components
    (stateOffset, _, stateNative) = intVectorNativeView states

instance CInput ComponentStateSequence where
    type CInputType ComponentStateSequence result =
        Int -> Int -> NativeVector Int -> Int -> NativeVector Int -> result
    withCInput sequence continuation =
        case componentStateSequenceNativeView sequence of
          (count, componentOffset, componentOwner, stateOffset, stateOwner) ->
              continuation count componentOffset componentOwner
                           stateOffset stateOwner

-- Project the state array without copying or inspecting the component array.
componentStates :: ComponentStateSequence -> U.Vector Int
componentStates (ComponentStateSequence values) = snd (U.unzip values)

foreign import bpcall "Alignment:leaf_sequence_counts" builtin_leaf_sequence_counts :: AlignmentMatrix -> Int -> EVector Int -> EVector (EVector Int)

branch_hmms (model,_) tree scale = getUEdgesSet tree & IntMap.fromSet (model $ fmap (scale *) $ branchLengthsSet tree)

seqlength as tree node = pairwise_alignment_length1 (as IntMap.! b) where
    b = head $ edgesOutOfNode tree node

-- We can't just do forall t.AlignmentOnTree t, because then any constraints on t will be on existential variables, resulting in ambiguity.
data AlignmentOnTree t = AlignmentOnTree t Int (IntMap Int) (IntMap PairwiseAlignment)

pairwiseAlignments (AlignmentOnTree _ _ _ as) = as

instance IsGraph t => Alignment (AlignmentOnTree t) where
    alignmentLength a@(AlignmentOnTree t _ _ as) = (sequenceLength a node0) + sum [numInsert (as IntMap.! b) | b <- allEdgesFromNode t node0]
                                                       where node0 = head $ getNodes t
    numSequences   (AlignmentOnTree _ n _  _) = n
    sequenceLength (AlignmentOnTree t n ls as) node =
        case IntMap.lookup node ls of Just length -> length
                                      Nothing -> case edgesOutOfNode t node of
                                                   (b:_) -> pairwise_alignment_length1 (as IntMap.! b)
                                                   _ -> error "sequenceLength: no length for degree-0 node!"


mkSequenceLengthsMap a@(AlignmentOnTree tree _ _ _) = getNodesSet tree & IntMap.fromSet (\node -> sequenceLength a node)

type EAlignment = EVector (EPair CPPString (EVector Int))
toEAlignment a = toVector [ c_pair (Text.toCppString label) indices | (label, indices) <- a ]
fromEAlignment ea = map ( (\(x,y) -> (Text.fromCppString x,y)) . pair_from_c) $ vectorToList ea

-- Current a' is an alignment, while counts and mapping use contiguous native
-- Int storage wrapped with their Haskell dimensions below.
foreign import bpcall "Alignment:compress_alignment" builtin_compress_alignment :: EAlignment ->
                                                                                   EPair EAlignment (EPair (NativeVector Int) (NativeVector Int))
-- Wrap both native integer results while retaining the compressed alignment's
-- existing runtime representation.
compressAlignment a = (compressed, counts, mapping)
    where tmp123 = builtin_compress_alignment (toEAlignment a)
          (compressed', tmp23) = pair_from_c tmp123
          (countsNative, mappingNative) = pair_from_c tmp23
          counts = intVectorFromNative countsNative
          mapping = intVectorFromNative mappingNative
          compressed = fromEAlignment compressed'

-- Return compressed sequences with their unboxed column multiplicities.
foreign import bpcall "Alignment:compress_alignment_var_nonvar" builtin_compress_alignment_var_nonvar :: EAlignment -> Alphabet ->
                                                                                                         EPair EAlignment (NativeVector Int)
-- Wrap the native multiplicities paired with the expanded compressed rows.
compressAlignmentVarNonvar a alphabet = (compressed, counts)
    where tmp12 = builtin_compress_alignment_var_nonvar (toEAlignment a) alphabet
          (compressed', countsNative) = pair_from_c tmp12
          counts = intVectorFromNative countsNative
          compressed = fromEAlignment compressed'

totalNumIndels (AlignmentOnTree t _ _ as) = sum [numIndels (as IntMap.! b) | b <- allEdgesFromNode t node0]
                                         where node0 = head $ getNodes t

totalLengthIndels (AlignmentOnTree t _ _ as) = sum [lengthIndels (as IntMap.! b) | b <- allEdgesFromNode t node0]
                                         where node0 = head $ getNodes t

-- Alignment -> Int -> EVector Int -> [EVector Int]
leaf_sequence_counts a n counts = vectorToList $ builtin_leaf_sequence_counts a n counts

{-
  OK, so what do we use the original alignment matrix for?
  * the alphabet
  * the original sequences.

  Right now, I think the node names still correspond to the input alignment names.
  But when the node names stop being [0..n_sequences-1], we will need a way to
  1. construct the alignment from a list of Sequence objects
  2. get a list (leafNodes ++ internalNodes)
  3. for each node, get the leaf label
  4. use smap to project the state to an alphabet letter.
  5. turn the list of Sequence objects into an alignment.
Maybe we want a map from String -> EVector Int.

That would lose the comments on the fastas though.
-}

foreign import bpcall "Alignment:select_alignment_columns" builtin_select_alignment_columns :: AlignmentMatrix -> Int -> Int -> NativeVector Int -> AlignmentMatrix
select_alignment_columns alignment sites =
    builtin_select_alignment_columns alignment offset count native
  where
    (offset, count, native) = intVectorNativeView (U.fromList sites)

foreign import bpcall "Alignment:select_alignment_pairs" builtin_select_alignment_pairs :: AlignmentMatrix -> EVector (EPair Int Int) -> Alphabet -> AlignmentMatrix
select_alignment_pairs alignment sites doublets = builtin_select_alignment_pairs alignment sites' doublets
    where sites' = toVector $ map (\(x,y) -> c_pair x y) sites

alignmentOnTreeFromSequences tree (Aligned sequences) = AlignmentOnTree tree numSequences lengths pairwiseAs
    where -- observedSequences :: IntMap (Maybe (EVector Int))
          observedSequences = labelToNodeMap tree $ getSequences sequences
          observedMasks = fmap (fmap bitmaskFromSequence') observedSequences
          -- What is going on here with addAllMissingAncestors?
          allSequences = minimallyConnectCharacters observedMasks tree (addAllMissingAncestors observedSequences tree)
          numSequences = length $ getSequences sequences
          lengths = getNodesSet tree & IntMap.fromSet (\node -> vector_size $ stripGaps $ allSequences IntMap.! node)
          bits = fmap bitmaskFromSequence' allSequences
          alignmentForBranch b = pairwise_alignment_from_bits (bits IntMap.! source) (bits IntMap.! target)
                                 where source = sourceNode tree b
                                       target = targetNode tree b
          pairwiseAs = getEdgesSet tree & IntMap.fromSet alignmentForBranch


find_sequence label sequences = find (\s -> fst s == label) sequences

-- Get a map from labeled nodes to Just v, and unlabeled nodes to Nothing.
-- Complain if a labeled node doesn't have a corresponding entry in the Map.
labelToNodeMap :: (IsGraph t, Eq (LabelType t)) => t -> [(LabelType t, v)] -> IntMap (Maybe v)
labelToNodeMap tree things = getNodesSet tree & IntMap.fromSet objectForNode where
    objectForNode node = case getLabel tree node of
                           Nothing -> Nothing
                           Just label ->
                               case lookup label things of
                                 Just object -> Just object
                                 Nothing -> error $ "No object for node with a certain label"

getSequencesOnTree :: (IsGraph t, LabelType t ~ Text) => [Sequence] -> t -> IntMap (Maybe Sequence)
getSequencesOnTree sequence_data tree = getNodesSet tree & IntMap.fromSet sequence_for_node where
    sequence_for_node node = case getLabel tree node of
                               Nothing ->  Nothing
                               Just label ->
                                   case find_sequence label sequence_data of
                                     Just sequence -> Just sequence
                                     Nothing -> error $ "No such sequence " ++ Text.unpack label

getLabelled :: IsGraph t => t -> (LabelType t -> a -> b) -> IntMap a -> [b]
getLabelled tree f things = catMaybes $ fmap go $ IntMap.toList things where
    go (node, thing) = case getLabel tree node of
                         Just label -> Just $ f label thing
                         Nothing -> Nothing

sequencesFromTree tree isequences = [(label, isequence) | n <- leafNodes tree ++ internalNodes tree,
                                                             let label = addAncestralLabel n (getLabels tree),
                                                             let isequence = isequences IntMap.! n]

labeledNodeMap tree objects = Map.fromList [(label, objects IntMap.! n) | n <- nodes tree, Just label <- [getLabels tree IntMap.! n]]

instance Show ComponentStateSequence where
    show (ComponentStateSequence values) = show values


-- Ideally we'd like to do
--    type NodeAlignment = EPair Int BranchAlignments
-- but this creates recursive type synonyms, which are not allowed.

-- We hack around this by removing the definition of NodeAlignment.
-- Then NodeAlignment needs a foreign import to construct it.

-- This constructs an all-insert alignment.
data NodeAlignment -- NodeAlignment SourceNode Int (EVector BranchAlignment)
foreign import bpcall "Alignment:" mkNodeAlignment :: Int -> Int -> EVector BranchAlignment -> NodeAlignment

data BranchAlignment -- BranchAlignment TargetNode PairwiseAlignment (EVector BranchAlignment)
instance RuntimeValue BranchAlignment
foreign import bpcall "Alignment:" mkBranchAlignment :: Int -> PairwiseAlignment -> EVector BranchAlignment -> BranchAlignment

-- Export the recursive branch structure while consuming graph edge views
-- directly from their unboxed representation.
exportAlignmentOnTree :: IsTree t => AlignmentOnTree t -> NodeAlignment
exportAlignmentOnTree a@(AlignmentOnTree tree _ _ as) = mkNodeAlignment root (sequenceLength a root) (branchAlignments $ edgesOutOfNodeVector tree root)
    where root = head $ getNodes tree
          branchAlignments edges = toVector [ mkBranchAlignment (targetNode tree e) (as IntMap.! e) (branchAlignments $ edgesAfterEdgeVector tree e) | e <- U.toList edges]

foreign import bpcall "Alignment:" substituteLetters :: EVector Int -> EVector Int -> EVector Int

foreign import bpcall "Alignment:" constructPositionSequencesRaw :: NodeAlignment -> EIntMap (EVector Int)
constructPositionSequences a = FIM.importIntMap $ constructPositionSequencesRaw $ exportAlignmentOnTree a

alignedSequences alignment@(AlignmentOnTree tree _ _ _) sequences = getNodesSet tree & IntMap.fromSet stateSequenceForNode
    where positionSequences = constructPositionSequences alignment
          stateSequenceForNode n = substituteLetters (sequences IntMap.! n) (positionSequences IntMap.! n)

class ToFasta a where
    toFasta :: a -> Text

instance ToFasta CharacterData where
    toFasta (CharacterData a sequences) = fastaSeqs [(label,sequenceToText a sequence) | (label,sequence) <- sequences]

instance ToFasta UnalignedCharacterData where
    toFasta (Unaligned d) = toFasta d

instance ToFasta AlignedCharacterData where
    toFasta (Aligned d) = toFasta d

align alignment (Unaligned (CharacterData alphabet seqs)) = Aligned (CharacterData alphabet alignedSeqs)
    where AlignmentOnTree tree _ _ _ = alignment
          seqsOnTree = fromMaybe (error "No label") <$> labelToNodeMap tree seqs
          alignedSeqsOnTree = alignedSequences alignment seqsOnTree
          alignedSeqs = getLabelled tree (,) alignedSeqsOnTree

instance Alignment AlignedCharacterData where
    alignmentLength (Aligned (CharacterData _ seqs)) = vector_size $ snd $ head seqs
    numSequences (Aligned (CharacterData _ seqs)) = length seqs
    sequenceLength (Aligned (CharacterData _ seqs)) index = vector_size $ stripGaps $ snd $ (seqs !! index)


-- In both cases we normalize the oldest taxon to have time 0.
-- Would we ever want to specify the present as being older that the oldest taxon?
data TimeDirection = Forward | Backward

foreign import bpcall "Alignment:" getTaxonAgesRaw :: EVector CPPString -> CPPString -> Int -> NativeVector Double

getTaxonAges labels regex direction = zip labels (U.toList ages)
    where cppLabels = toVector (map Text.toCppString labels)
          cppRegex = pack_cpp_string regex
          convert Forward  = 0
          convert Backward = 1
          cppDirection = convert direction
          ages = doubleVectorFromNativeWithLength (length labels)
                                                   (getTaxonAgesRaw cppLabels cppRegex cppDirection)

class AncestralAlignment a where
    ancestralAlignment :: (IsTree t, LabelType t ~ Text) => t -> a -> EVector Int -> Alphabet -> IntMap ComponentStateSequence -> AlignedCharacterData

instance AncestralAlignment (IntMap (Maybe BitVector)) where
    ancestralAlignment tree observedMasks smap alphabet componentStateSequences =
--    This also needs the map from columns to compressed columns:
      let ancestralLetterSequences = statesToLetters smap . componentStates <$> componentStateSequences
          connectedLetterSequences = minimallyConnectCharacters observedMasks tree ancestralLetterSequences
      in Aligned (CharacterData alphabet $ sequencesFromTree tree connectedLetterSequences)


instance IsTree t => AncestralAlignment (AlignmentOnTree t) where
    ancestralAlignment rtree alignment smap alphabet componentStateSequences =
        Aligned $ CharacterData alphabet (sequencesFromTree rtree alignedLetterSequences)
        where letterSequences = statesToLetters smap . componentStates <$> componentStateSequences
              alignedLetterSequences = alignedSequences alignment letterSequences

leafAlignment tree sequenceData = labelToNodeMap tree $ fmap (fmap bitmaskFromSequence') $ getSequences sequenceData


foreign import bpcall "Foreign:" encodeComponentStateSequenceRaw :: Int -> Int -> NativeVector Int -> Int -> NativeVector Int -> CPPString

-- Encode directly from both native child views, retaining the historical
-- missing-state sentinel without constructing an intermediate pair list.
encodeComponentStateSequence sequence = Text.fromCppString $
    encodeComponentStateSequenceRaw count componentOffset componentNative
                                              stateOffset stateNative
  where
    (count, componentOffset, componentNative, stateOffset, stateNative) =
        componentStateSequenceNativeView sequence

instance ToJSON ComponentStateSequence where
    toJSON = error "ComponentStateSequence.toJSON: not implemented"

    toEncoding = E.unsafeToEncoding . encodeComponentStateSequence
