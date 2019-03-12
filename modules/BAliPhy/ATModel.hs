module BAliPhy.ATModel where

data PairwiseAlignment
data AlignmentOnTree = AlignmentOnTree [PairwiseAlignment]
data Partition = Partition Tree AlignmentOnTree
topology  (Partition t _) = t
alignment (Partition _ a) = a

-- Assumptions FIXME: branch lengths -- we could have multiple set of them.
--                                      what we really need is transition probabilities for each partition.

data IndelModel = IndelModel
data ATModel = ATModel Tree [MixtureModels] [IndelModel] [Double] [Double]
tree           (ATModel t _  _ _ _) = t
smodels        (ATModel _ ss _ _ _) = ss
imodels        (ATModel _ _ is _ _) = is
scales         (ATModel _ _ _ rs _) = rs
branch_lengths (ATModel _ _ _ _ ls) = ls
