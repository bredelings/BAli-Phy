module SModel.Parsimony where

import Tree
import Bio.Sequence
import Bio.Alphabet
import Bio.Alignment
import Data.BitVector
import Data.Foldable
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data (NativeMatrix, matrixFromNative, nativeMatrix)
import Foreign.Vector
import Numeric.LogDouble
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (intVectorNativeView)
import Foreign.NativeVector (NativeVector)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data CondPars

type MutCosts = Matrix Int

foreign import bpcall "Parsimony:unitCostMatrix" unitCostMatrixNative :: Alphabet -> NativeMatrix Int
foreign import bpcall "Parsimony:aminoAcidCostMatrix" aminoAcidCostMatrixNative :: Alphabet -> NativeMatrix Int

foreign import bpcall "Parsimony:pos1CostMatrix" pos1CostMatrixNative :: Alphabet -> NativeMatrix Int
foreign import bpcall "Parsimony:pos2CostMatrix" pos2CostMatrixNative :: Alphabet -> NativeMatrix Int

foreign import bpcall "Parsimony:peelMuts" peelMutsNative :: EVector (EVector Int) -> Alphabet -> EVector PairwiseAlignment -> EVector CondPars -> NativeMatrix Int -> CondPars
foreign import bpcall "Parsimony:mutsRoot" mutsRootNative :: EVector (EVector Int) -> Alphabet -> EVector PairwiseAlignment -> EVector CondPars -> NativeMatrix Int -> Int

unitCostMatrix alphabet = costMatrix alphabet (unitCostMatrixNative alphabet)
aminoAcidCostMatrix alphabet = costMatrix alphabet (aminoAcidCostMatrixNative alphabet)
pos1CostMatrix alphabet = costMatrix alphabet (pos1CostMatrixNative alphabet)
pos2CostMatrix alphabet = costMatrix alphabet (pos2CostMatrixNative alphabet)

costMatrix alphabet = matrixFromNative dimension dimension
  where dimension = alphabetSize alphabet

peelMuts sequences alphabet alignments partials costs =
    peelMutsNative sequences alphabet alignments partials (nativeMatrix costs)

mutsRoot sequences alphabet alignments partials costs =
    mutsRootNative sequences alphabet alignments partials (nativeMatrix costs)


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
    parsimony tree costs (sequenceData,alignment) = let as = pairwiseAlignments alignment
                                                        alphabet = getAlphabet sequenceData
                                                        maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
                                                    in parsimony_root tree maybeNodeSequences as alphabet costs

----
type ColumnCounts = U.Vector Int

foreign import bpcall "Parsimony:peelMutsFixedA" peelMutsFixedANative :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector CondPars -> NativeMatrix Int -> CondPars
foreign import bpcall "Parsimony:mutsRootFixedA" mutsRootFixedARaw :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector CondPars -> NativeMatrix Int -> Int -> Int -> NativeVector Int -> Int

peelMutsFixedA sequences alphabet partials costs =
    peelMutsFixedANative sequences alphabet partials (nativeMatrix costs)

mutsRootFixedA sequences alphabet partials costs counts =
    mutsRootFixedARaw sequences alphabet partials (nativeMatrix costs) offset count native
  where
    (offset, count, native) = intVectorNativeView counts

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
