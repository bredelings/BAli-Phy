module Bio.Alignment.Pairwise where

import Data.BitVector

data PairwiseAlignment = PairwiseAlignment

numInsert :: PairwiseAlignment -> Int
builtin numInsert 1 "numInsert" "Alignment"

numMatch :: PairwiseAlignment -> Int
builtin numMatch 1 "numMatch" "Alignment"

numDelete :: PairwiseAlignment -> Int
builtin numDelete 1 "numDelete" "Alignment"

pairwise_alignment_length1 :: PairwiseAlignment -> Int
builtin pairwise_alignment_length1 1 "pairwise_alignment_length1" "Alignment"

pairwise_alignment_length2 :: PairwiseAlignment -> Int
builtin pairwise_alignment_length2 1 "pairwise_alignment_length2" "Alignment"

builtin_pairwise_alignment_from_bits :: CBitVector -> CBitVector -> PairwiseAlignment
builtin builtin_pairwise_alignment_from_bits 2 "pairwise_alignment_from_bits" "Bits"

pairwise_alignment_from_bits :: BitVector -> BitVector -> PairwiseAlignment
pairwise_alignment_from_bits (BitVector x) (BitVector y) = builtin_pairwise_alignment_from_bits x y

unaligned_pairwise_alignment :: Int -> Int -> PairwiseAlignment
builtin unaligned_pairwise_alignment 2 "unaligned_pairwise_alignment" "Alignment"

left_aligned_pairwise_alignment :: Int -> Int -> PairwiseAlignment
builtin left_aligned_pairwise_alignment 2 "left_aligned_pairwise_alignment" "Alignment"

flip_alignment :: PairwiseAlignment -> PairwiseAlignment
builtin flip_alignment 1 "flip_alignment" "Alignment"

