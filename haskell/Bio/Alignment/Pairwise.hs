module Bio.Alignment.Pairwise where

import Data.BitVector

data PairwiseAlignment = PairwiseAlignment

numInsert :: PairwiseAlignment -> Int
builtin "Alignment:numInsert" numInsert 1

numMatch :: PairwiseAlignment -> Int
builtin "Alignment:numMatch" numMatch 1

numDelete :: PairwiseAlignment -> Int
builtin "Alignment:numDelete" numDelete 1

pairwise_alignment_length1 :: PairwiseAlignment -> Int
builtin "Alignment:pairwise_alignment_length1" pairwise_alignment_length1 1

pairwise_alignment_length2 :: PairwiseAlignment -> Int
builtin "Alignment:pairwise_alignment_length2" pairwise_alignment_length2 1

builtin_pairwise_alignment_from_bits :: CBitVector -> CBitVector -> PairwiseAlignment
builtin "Bits:pairwise_alignment_from_bits" builtin_pairwise_alignment_from_bits 2

pairwise_alignment_from_bits :: BitVector -> BitVector -> PairwiseAlignment
pairwise_alignment_from_bits (BitVector x) (BitVector y) = builtin_pairwise_alignment_from_bits x y

unaligned_pairwise_alignment :: Int -> Int -> PairwiseAlignment
builtin "Alignment:unaligned_pairwise_alignment" unaligned_pairwise_alignment 2

left_aligned_pairwise_alignment :: Int -> Int -> PairwiseAlignment
builtin "Alignment:left_aligned_pairwise_alignment" left_aligned_pairwise_alignment 2

flip_alignment :: PairwiseAlignment -> PairwiseAlignment
builtin "Alignment:flip_alignment" flip_alignment 1

