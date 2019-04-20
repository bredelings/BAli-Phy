module BAliPhy.ATModel.DataPartition where

import Tree

data LeafSequence
data ConditionalLikelihoodVector
data PairwiseAlignment
data Partition = Partition Tree (Array Int LeafSequence) [PairwiseAlignment] (Array Int ConditionalLikelihoodVector) 
--tree           (Partition t _  _ _ ) = t
leaf_sequences      (Partition _  ls _  _ ) = ls
pairwise_alignments (Partition _  _  as _ ) = as
