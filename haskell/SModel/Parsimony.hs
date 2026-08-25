module SModel.Parsimony where

import Tree
import Bio.Sequence
import Bio.Alphabet
import Bio.Alignment
import Compiler.FFI.Import (COutput)
import Compiler.FFI.Runtime (RuntimeValue)
import Data.Bit.Internal (CBitVector, bitVectorNativeOwner)
import Data.Foldable
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data (NativeMatrix, matrixFromNative)
import Foreign.Vector
import Numeric.Log
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as U

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data CondPars

instance RuntimeValue CondPars
instance COutput CondPars

type MutCosts = Matrix Int

foreign import bpcall "Parsimony:unitCostMatrix" unitCostMatrixNative :: Alphabet -> NativeMatrix Int
foreign import bpcall "Parsimony:aminoAcidCostMatrix" aminoAcidCostMatrixNative :: Alphabet -> NativeMatrix Int

foreign import bpcall "Parsimony:pos1CostMatrix" pos1CostMatrixNative :: Alphabet -> NativeMatrix Int
foreign import bpcall "Parsimony:pos2CostMatrix" pos2CostMatrixNative :: Alphabet -> NativeMatrix Int

-- TEMPORARY EVECTOR ADAPTER: parsimony still consumes collections of boxed sequence rows.
-- Remove these conversions when the raw interfaces accept unboxed rows.
foreign import trcall "Parsimony:peelMuts" peelMutsRaw :: EVector (EVector Int) -> Alphabet -> AmbiguityDatabase -> EVector PairwiseAlignment -> EVector CondPars -> Matrix Int -> CondPars
foreign import trcall "Parsimony:mutsRoot" mutsRootRaw :: EVector (EVector Int) -> Alphabet -> AmbiguityDatabase -> EVector PairwiseAlignment -> EVector CondPars -> Matrix Int -> Int

unitCostMatrix alphabet = costMatrix alphabet (unitCostMatrixNative alphabet)
aminoAcidCostMatrix alphabet = costMatrix alphabet (aminoAcidCostMatrixNative alphabet)
pos1CostMatrix alphabet = costMatrix alphabet (pos1CostMatrixNative alphabet)
pos2CostMatrix alphabet = costMatrix alphabet (pos2CostMatrixNative alphabet)

costMatrix alphabet = matrixFromNative dimension dimension
  where dimension = alphabetSize alphabet

peelMuts sequences alphabet ambiguities alignments partials costs =
    peelMutsRaw (toVector $ map toLegacySequenceVector sequences) alphabet ambiguities alignments partials costs

mutsRoot sequences alphabet ambiguities alignments partials costs =
    mutsRootRaw (toVector $ map toLegacySequenceVector sequences) alphabet ambiguities alignments partials costs


class Parsimony a where
    parsimony :: (IsTree t, LabelType t ~ Text) => t -> MutCosts -> a -> Int


cached_conditional_muts t seqs as alpha ambiguities cost =
    let pc = IntMap.fromSet pcf $ getEdgesSet t
        pcf b = let inEdges = edgesBeforeEdgeSet t b
                    cpsIn = IntMap.restrictKeysToVector pc inEdges
                    asIn = IntMap.restrictKeysToVector as inEdges
                    node = sourceNode t b
                    sequences = maybeToList $ seqs IntMap.! node
                in peelMuts sequences alpha ambiguities asIn cpsIn cost
    in pc

peel_muts t cp as root seqs alpha ambiguities cost =
    let inEdges = edgesTowardNodeSet t root
        cpsIn = IntMap.restrictKeysToVector cp inEdges
        asIn = IntMap.restrictKeysToVector as inEdges
        sequences = maybeToList $ seqs IntMap.! root
    in mutsRoot sequences alpha ambiguities asIn cpsIn cost

parsimony_root t seqs as alpha ambiguities cost =
    let pc = cached_conditional_muts t seqs as alpha ambiguities cost
        root = head $ getNodes t
    in peel_muts t pc as root seqs alpha ambiguities cost

instance Parsimony (UnalignedCharacterData, AlignmentOnTree t) where
    parsimony tree costs (sequenceData,alignment) = let as = pairwiseAlignments alignment
                                                        alphabet = getAlphabet sequenceData
                                                        ambiguities = getAmbiguities sequenceData
                                                        maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
                                                    in parsimony_root tree maybeNodeSequences as alphabet ambiguities costs

----
type ColumnCounts = U.Vector Int

-- TEMPORARY EVECTOR ADAPTER: fixed parsimony still embeds boxed rows in EPairs.
-- Remove these conversions when the raw interfaces accept unboxed rows and masks separately.
foreign import trcall "Parsimony:peelMutsFixedA" peelMutsFixedARaw :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> AmbiguityDatabase -> EVector CondPars -> Matrix Int -> CondPars
foreign import trcall "Parsimony:mutsRootFixedA" mutsRootFixedARaw :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> AmbiguityDatabase -> EVector CondPars -> Matrix Int -> U.Vector Int -> Int

peelMutsFixedA sequences alphabet ambiguities partials costs =
    peelMutsFixedARaw (toVector $ map legacyPair sequences) alphabet ambiguities partials costs
  where
    legacyPair (sequence, mask) = c_pair (toLegacySequenceVector sequence) (bitVectorNativeOwner mask)

mutsRootFixedA sequences alphabet ambiguities partials costs counts =
    mutsRootFixedARaw (toVector $ map legacyPair sequences) alphabet ambiguities partials costs counts
  where
    legacyPair (sequence, mask) = c_pair (toLegacySequenceVector sequence) (bitVectorNativeOwner mask)

cached_conditional_muts_fixed_A t seqs alpha ambiguities cost =
    let pc    = IntMap.fromSet pcf $ getEdgesSet t
        pcf b = let inEdges = edgesBeforeEdgeSet t b
                    clsIn = IntMap.restrictKeysToVector pc inEdges
                    node = sourceNode t b
                    sequences = maybeToList $ seqs IntMap.! node
                in peelMutsFixedA sequences alpha ambiguities clsIn cost
    in pc

peel_muts_fixed_A t cp root seqs alpha ambiguities cost counts =
    let inEdges = edgesTowardNodeSet t root
        clsIn = IntMap.restrictKeysToVector cp inEdges
        sequences = maybeToList $ seqs IntMap.! root
    in mutsRootFixedA sequences alpha ambiguities clsIn cost counts

parsimony_root_fixed_A t seqs alpha ambiguities cost counts =
    let pc = cached_conditional_muts_fixed_A t seqs alpha ambiguities cost
        root = head $ getNodes t
    in peel_muts_fixed_A t pc root seqs alpha ambiguities cost counts

instance Parsimony AlignedCharacterData where
    parsimony tree cost alignment = let (isequences, columnCounts, mapping) = compressAlignment $ getSequences alignment
                                        maybeNodeISequences = labelToNodeMap tree isequences
                                        maybeNodeSeqsBits = ((\seq -> (stripGaps seq, bitmaskFromSequence seq)) <$>) <$> maybeNodeISequences
                                        alphabet = getAlphabet alignment
                                        ambiguities = getAmbiguities alignment
                                    in parsimony_root_fixed_A tree maybeNodeSeqsBits alphabet ambiguities cost columnCounts

{-
parsimony_SEV :: IsTree t => t -> IntMap (EVector Int) -> IntMap PairwiseAlignment -> Alphabet -> MutCosts -> Int
parsimony_SEV t seqs as alpha cost = let pc = cached_conditional_muts_SEV t seqs as alpha cost
                                        root = head $ getNodes t
                                     in peel_muts_SEV t pc as root seqs alpha cost
-}
