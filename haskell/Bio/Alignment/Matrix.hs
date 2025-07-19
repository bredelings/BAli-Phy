module Bio.Alignment.Matrix where

import Bio.Sequence
import Bio.Alphabet
import Data.BitVector
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import Tree
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Control.Monad (liftM2)
import Data.Maybe (catMaybes)
import Data.Foldable (toList)

data AlignmentMatrix

foreign import bpcall "Alignment:alignment_length" alignment_length :: AlignmentMatrix -> Int

foreign import bpcall "Alignment:load_alignment" builtin_load_alignment :: Alphabet -> CPPString -> IO AlignmentMatrix

load_alignment :: Alphabet -> String -> IO AlignmentMatrix
load_alignment alphabet filename = builtin_load_alignment alphabet (list_to_string filename)

foreign import bpcall "Alignment:alignment_from_sequences" builtin_alignment_from_sequences :: Alphabet -> EVector (EPair CPPString CPPString) -> AlignmentMatrix

alignment_from_sequences :: Alphabet -> [Sequence] -> AlignmentMatrix
alignment_from_sequences a seqs = builtin_alignment_from_sequences a (toVector $ fmap (\(n, s) -> c_pair (Text.toCppString n) (Text.toCppString s)) seqs)


foreign import bpcall "Alignment:sequence_names" builtin_sequence_names :: AlignmentMatrix -> EVector CPPString
sequence_names :: AlignmentMatrix -> [Text]
sequence_names a = map Text.fromCppString $ vectorToList $ builtin_sequence_names a

foreign import bpcall "Alignment:sequences_from_alignment" builtin_indices_from_alignment :: AlignmentMatrix -> EVector (EVector Int)
indices_from_alignment :: AlignmentMatrix -> [ EVector Int ]
indices_from_alignment a = vectorToList $ builtin_indices_from_alignment a

-- use isequences instead of "sequences" since we aren't using C++ Box<sequence> here.
isequences_from_alignment a = zip (sequence_names a) (indices_from_alignment a)


foreign import bpcall "Alignment:reorder_alignment" builtin_reorder_alignment :: EVector CPPString -> AlignmentMatrix -> AlignmentMatrix
reorder_alignment :: [String] -> AlignmentMatrix -> AlignmentMatrix
reorder_alignment names a = builtin_reorder_alignment names' a where names' = toVector $ map pack_cpp_string names

foreign import bpcall "Bits:alignment_row_to_presence_bitvector" builtin_alignment_row_to_bitvector :: AlignmentMatrix -> Int -> CBitVector
alignment_row_to_bitvector a row = BitVector $ builtin_alignment_row_to_bitvector a row

{-
  The idea behind getConnectedStates:

  1. First we compute the characters behind each branch with any characters at the source node.

    - We use Maybe BitVector, so that we don't assume that nodes with no data are known
      to be all deleted.  Instead, use represent such nodes with Nothing, indicating that we don't
      know what's behind them.

    - We don't treat leaf nodes differently.  Instead, we combine data at the source node of
      a branch with data on previous branches.  On a leaf branch, then typically there's
      data at the node, and there are 0 incoming branches.  On an internal node, then typically
      there is no data at the node, but data on incoming branches.

    - If we have data at an internal node, then we COULD have a situation
      where we have 1 --> 0 <-- 1.  In this case, what should we do?

      This presumes that the character is deleted at the node, but present on both
      children, which is not allowed.  So, we assume this can't happen and just OR the bits.
-}

charactersBehind :: IsTree t => IntMap (Maybe BitVector) -> t -> IntMap (Maybe BitVector)
charactersBehind sequence_masks tree = let
    branches = tree & getEdgesSet

    combine (Just x) (Just y) = Just (x .|. y)
    combine (Just x) Nothing  = Just x
    combine Nothing (Just y)  = Just y
    combine Nothing Nothing   = Nothing

    charBehind'' = branches & IntMap.fromSet charBehind
    charBehind' e = charBehind'' IntMap.! e
    charBehind e = let nodeMask = sequence_masks IntMap.! sourceNode tree e
                       edgeMasks = fmap charBehind' (edgesBeforeEdge tree e)
                   in foldr combine nodeMask edgeMasks
    in charBehind''


{-
  2. Then determine which characters are present at each node:

  - If there are NO incoming branches with data AND there's no data at the node, then
    there's no data on the whole tree, and we don't know how long the vector should be,
    so we error out.

  - If there's only one incoming branch with data behind it, then we just use the bitmask
    for that branch.  This is supposed to handle cases like a degree-1 root node with no data.

    This is actually quadratic in the number of incoming branches...
    An alternative algorithm would be to create an EVector Int and increment the integer at
     each location for each bitmap with a 1.  Then we set each bit to 1 if (i>=2) and 0 otherwise.
     But this loses the bitwise speedup if there are only three incoming edges, which is pretty common.

  - If there are two or more incoming branches with data behind them, then a character is
    presentat the node if its present behind at least two incoming branches.
-}


{- Issues:
   (a) This is quadritic in the node degree.
       We could fix this by COUNTING the number of bits at a node and recording a 1 if there are 2 or more.
       greaterThanBits 1 $ foldr1 addBits (zeroBits L) ms
   (b) Its not clear how to handle observed data at internal nodes.
       Suppose we have observed data at all nodes.  And suppose that we have "-" -> "N" -> "-".
       What should we do?
       Currently what we do is to treat an observed - as - even if there are Ns on both sides.
 -}
getConnectedStates :: IsTree t => IntMap (Maybe BitVector) -> t -> IntMap BitVector
getConnectedStates sequence_masks tree =
    let nodes = tree & getNodesSet

        node_masks = nodes & IntMap.fromSet mask_for_node

        charBehind = charactersBehind sequence_masks tree
        charBehind' e = charBehind IntMap.! e

        pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

        mask_for_node node = case sequence_masks IntMap.! node of
                               Just mask -> mask
                               Nothing -> let edgesin = edgesTowardNode tree node
                                              inputs = catMaybes $ toList $ fmap charBehind' edgesin
                                          in case inputs of [] -> error $ "get_connected_states: No sequences at or behind node " ++ show node
                                                            [m] -> m
                                                            ms -> foldr1 (.|.) [m1 .&. m2 | (m1,m2) <- pairs ms]
     in node_masks

minimallyConnectCharacters leafSequences tree allSequences = nodes & IntMap.fromSet sequenceForNode
    where sequenceForNode n = maskSequence (nodeMasks IntMap.! n) (allSequences IntMap.! n)
          leafMasks = fmap (fmap bitmaskFromSequence') leafSequences
          nodeMasks = getConnectedStates leafMasks tree
          nodes = tree & getNodesSet

{- Here we create fake sequences at internal nodes that are entirely composed of Ns, with no gaps. -}
addAllMissingAncestors observedSequences tree = fromMaybe missingSequence <$> observedSequences
    where missingSequence = toVector $ replicate alignmentLength missingCharIndex
          alignmentLength = fromMaybe (error msg) $ allSame $ observedSequenceLengths
          msg = "minimallyConnectCharacters': not all observed sequences are the same length!"
          observedSequenceLengths = vector_size <$> (catMaybes $ IntMap.elems observedSequences)

