module BAliPhy.ATModel.DataPartition where

import Data.Maybe
import Tree
import Alignment

data IModel
data SModel
data HMM
data PairwiseAlignment
data Partition = Partition SModel (Maybe IModel) Double Tree AlignmentOnTree (Maybe (Array Int HMM))

smodel              (Partition s _ _ _  _  _ ) = s
imodel              (Partition _ i _ _  _  _ ) = i
scale               (Partition _ _ s _  _  _ ) = s
get_tree            (Partition _ _ _ t  _  _ ) = t
get_alignment       (Partition _ _ _ _  as _ ) = as
hmms                (Partition _ _ _ _  _  hs) = hs
