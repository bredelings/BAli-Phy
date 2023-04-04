module Bio.Alignment.Pairwise where

import Data.BitVector
import Control.DeepSeq
import Numeric.LogDouble

data PairHMM

foreign import bpcall "Alignment:" transition_counts :: PairwiseAlignment -> Matrix Int

foreign import bpcall "Alignment:" pairwise_alignment_probability_from_counts :: Matrix Int -> PairHMM -> LogDouble

data PairwiseAlignment = PairwiseAlignment

foreign import bpcall "Alignment:numInsert" numInsert :: PairwiseAlignment -> Int

foreign import bpcall "Alignment:numMatch" numMatch :: PairwiseAlignment -> Int

foreign import bpcall "Alignment:numDelete" numDelete :: PairwiseAlignment -> Int

foreign import bpcall "Alignment:pairwise_alignment_length1" pairwise_alignment_length1 :: PairwiseAlignment -> Int

foreign import bpcall "Alignment:pairwise_alignment_length2" pairwise_alignment_length2 :: PairwiseAlignment -> Int

foreign import bpcall "Bits:pairwise_alignment_from_bits" builtin_pairwise_alignment_from_bits :: CBitVector -> CBitVector -> PairwiseAlignment

pairwise_alignment_from_bits :: BitVector -> BitVector -> PairwiseAlignment
pairwise_alignment_from_bits (BitVector x) (BitVector y) = builtin_pairwise_alignment_from_bits x y

foreign import bpcall "Alignment:unaligned_pairwise_alignment" unaligned_pairwise_alignment :: Int -> Int -> PairwiseAlignment

foreign import bpcall "Alignment:left_aligned_pairwise_alignment" left_aligned_pairwise_alignment :: Int -> Int -> PairwiseAlignment

foreign import bpcall "Alignment:flip_alignment" flip_alignment :: PairwiseAlignment -> PairwiseAlignment

instance NFData PairHMM

instance NFData PairwiseAlignment
