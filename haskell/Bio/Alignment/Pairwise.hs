module Bio.Alignment.Pairwise where

import Data.BitVector

data PairwiseAlignment = PairwiseAlignment

numInsert :: PairwiseAlignment -> Int
builtin numInsert 1 "Alignment:numInsert"

numMatch :: PairwiseAlignment -> Int
builtin numMatch 1 "Alignment:numMatch"

numDelete :: PairwiseAlignment -> Int
builtin numDelete 1 "Alignment:numDelete"

pairwise_alignment_length1 :: PairwiseAlignment -> Int
builtin pairwise_alignment_length1 1 "Alignment:pairwise_alignment_length1"

pairwise_alignment_length2 :: PairwiseAlignment -> Int
builtin pairwise_alignment_length2 1 "Alignment:pairwise_alignment_length2"

builtin_pairwise_alignment_from_bits :: CBitVector -> CBitVector -> PairwiseAlignment
builtin builtin_pairwise_alignment_from_bits 2 "Bits:pairwise_alignment_from_bits"

pairwise_alignment_from_bits :: BitVector -> BitVector -> PairwiseAlignment
pairwise_alignment_from_bits (BitVector x) (BitVector y) = builtin_pairwise_alignment_from_bits x y

unaligned_pairwise_alignment :: Int -> Int -> PairwiseAlignment
builtin unaligned_pairwise_alignment 2 "Alignment:unaligned_pairwise_alignment"

left_aligned_pairwise_alignment :: Int -> Int -> PairwiseAlignment
builtin left_aligned_pairwise_alignment 2 "Alignment:left_aligned_pairwise_alignment"

flip_alignment :: PairwiseAlignment -> PairwiseAlignment
builtin flip_alignment 1 "Alignment:flip_alignment"

