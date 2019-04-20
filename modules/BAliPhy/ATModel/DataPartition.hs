module BAliPhy.ATModel.DataPartition where

import Alignment
import Tree

data LeafSequence
data ConditionalLikelihoodVector
data PairwiseAlignment
data Partition = Partition Tree (Array Int LeafSequence) (Array Int ConditionalLikelihoodVector) (Array Int PairwiseAlignment)
--tree           (Partition t _  _ _ ) = t
leaf_sequences (Partition _ ls _ _ ) = ls
