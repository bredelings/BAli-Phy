module SModel.Parsimony where

import Tree
import Bio.Sequence
import Bio.Alphabet
import Bio.Alignment
import Data.BitVector
import Data.Foldable
import Data.Matrix
import Data.Array
import Foreign.Vector
import Numeric.LogDouble
import Data.Maybe (maybeToList)
import Data.Text (Text)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data CondPars

type MutCosts = Matrix Int

foreign import bpcall "Parsimony:" unitCostMatrix :: Alphabet -> MutCosts
foreign import bpcall "Parsimony:" aminoAcidCostMatrix :: Alphabet -> MutCosts

foreign import bpcall "Parsimony:" pos1CostMatrix :: Alphabet -> MutCosts
foreign import bpcall "Parsimony:" pos2CostMatrix :: Alphabet -> MutCosts

foreign import bpcall "Parsimony:" peelMuts :: EVector (EVector Int) -> Alphabet -> EVector PairwiseAlignment -> EVector CondPars -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" mutsRoot :: EVector (EVector Int) -> Alphabet -> EVector PairwiseAlignment -> EVector CondPars -> MutCosts -> Int


class Parsimony a where
    parsimony :: (IsTree t, LabelType t ~ Text) => t -> MutCosts -> a -> Int


cached_conditional_muts t seqs as alpha cost = let pc    = IntMap.fromSet pcf $ getEdgesSet t
                                                   pcf b = let inEdges = edgesBeforeEdgeSet t b
                                                               cpsIn = IntMap.restrictKeysToVector pc inEdges
                                                               asIn  = IntMap.restrictKeysToVector as inEdges
                                                               node = sourceNode t b
                                                               sequences = maybeToList $ seqs IntMap.! node
                                                           in peelMuts (toVector sequences) alpha asIn cpsIn cost
                                               in pc

peel_muts t cp as root seqs alpha cost = let inEdges = edgesTowardNodeSet t root
                                             cpsIn = IntMap.restrictKeysToVector cp inEdges
                                             asIn  = IntMap.restrictKeysToVector as inEdges
                                             sequences = maybeToList $ seqs IntMap.! root
                                         in mutsRoot (toVector sequences) alpha asIn cpsIn cost

parsimony_root t seqs as alpha cost = let pc = cached_conditional_muts t seqs as alpha cost
                                          root = head $ getNodes t
                                      in peel_muts t pc as root seqs alpha cost

instance Parsimony (UnalignedCharacterData, AlignmentOnTree t) where
    parsimony tree costs (sequenceData,alignment) = let as = pairwise_alignments alignment
                                                        alphabet = getAlphabet sequenceData
                                                        maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
                                                    in parsimony_root tree maybeNodeSequences as alphabet costs

----
type ColumnCounts = EVector Int

foreign import bpcall "Parsimony:" peelMutsFixedA :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector CondPars -> MutCosts -> CondPars
foreign import bpcall "Parsimony:" mutsRootFixedA :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector CondPars -> MutCosts -> EVector Int -> Int

cached_conditional_muts_fixed_A t seqs alpha cost =
    let pc    = IntMap.fromSet pcf $ getEdgesSet t
        pcf b = let inEdges = edgesBeforeEdgeSet t b
                    clsIn = IntMap.restrictKeysToVector pc inEdges
                    node = sourceNode t b
                    sequences = maybeToList $ c_pair' <$> seqs IntMap.! node
                in peelMutsFixedA (toVector sequences) alpha clsIn cost
    in pc

peel_muts_fixed_A t cp root seqs alpha cost counts = let inEdges = edgesTowardNodeSet t root
                                                         clsIn = IntMap.restrictKeysToVector cp inEdges
                                                         sequences = maybeToList $ c_pair' <$> seqs IntMap.! root
                                                     in mutsRootFixedA (toVector sequences) alpha clsIn cost counts

parsimony_root_fixed_A t seqs alpha cost counts = let pc = cached_conditional_muts_fixed_A t seqs alpha cost
                                                      root = head $ getNodes t
                                                  in peel_muts_fixed_A t pc root seqs alpha cost counts

instance Parsimony AlignedCharacterData where
    parsimony tree cost alignment = let (isequences, columnCounts, mapping) = compressAlignment $ getSequences alignment
                                        maybeNodeISequences = labelToNodeMap tree isequences
                                        maybeNodeSeqsBits = ((\seq -> (stripGaps seq, bitmaskFromSequence seq)) <$>) <$> maybeNodeISequences
                                        alphabet = getAlphabet alignment
                                    in parsimony_root_fixed_A tree maybeNodeSeqsBits alphabet cost columnCounts

{-
parsimony_SEV :: IsTree t => t -> IntMap (EVector Int) -> IntMap PairwiseAlignment -> Alphabet -> MutCosts -> Int
parsimony_SEV t seqs as alpha cost = let pc = cached_conditional_muts_SEV t seqs as alpha cost
                                        root = head $ getNodes t
                                     in peel_muts_SEV t pc as root seqs alpha cost
-}
