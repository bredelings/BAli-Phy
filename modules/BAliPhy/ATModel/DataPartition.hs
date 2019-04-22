module BAliPhy.ATModel.DataPartition where

import Data.Maybe
import Tree

data IModel
data SModel
data LeafSequence
data ConditionalLikelihoodVector
data PairwiseAlignment
data Partition = Partition SModel (Maybe IModel) Double Tree (Array Int LeafSequence) (Array Int PairwiseAlignment) (Array Int ConditionalLikelihoodVector) 

smodel              (Partition s _ _ _  _  _  _ ) = s
imodel              (Partition _ i _ _  _  _  _ ) = i
scale               (Partition _ _ s _  _  _  _ ) = s
get_tree            (Partition _ _ _ t  _  _  _ ) = t
leaf_sequences      (Partition _ _ _ _  ls _  _ ) = ls
pairwise_alignments (Partition _ _ _ _  _  as _ ) = as
