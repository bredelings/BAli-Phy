module BAliPhy.ATModel.DataPartition where

import Data.Maybe
import Tree
import Alignment

data IModel
data SModel
data HMM
data LeafSequence
data ConditionalLikelihoodVector
data PairwiseAlignment
data Matrix
data Partition = Partition SModel (Maybe IModel) Double Tree (Array Int LeafSequence) AlignmentOnTree (Maybe (Array Int HMM)) (Array Int Matrix) (Array Int ConditionalLikelihoodVector) Double

smodel              (Partition s _ _ _  _  _  _  _  _  _) = s
imodel              (Partition _ i _ _  _  _  _  _  _  _) = i
scale               (Partition _ _ s _  _  _  _  _  _  _) = s
get_tree            (Partition _ _ _ t  _  _  _  _  _  _) = t
leaf_sequences      (Partition _ _ _ _  ls _  _  _  _  _) = ls
get_alignment       (Partition _ _ _ _  _  as _  _  _  _) = as
hmms                (Partition _ _ _ _  _  _  hs _  _  _) = hs
transition_ps       (Partition _ _ _ _  _  _  _  ps _  _) = ps
cond_likes          (Partition _ _ _ _  _  _  _  _  cs _) = cs
likelihood          (Partition _ _ _ _  _  _  _  _  _ ll) = ll
