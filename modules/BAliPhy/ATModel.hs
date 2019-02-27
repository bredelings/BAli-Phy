module BAliPhy.ATModel where

data PairwiseAlignment
data AlignmentOnTree = AlignmentOnTree [PairwiseAlignment]
data Partition = Partition Tree AlignmentOnTree
topology  (Partition t _) = t
alignment (Partition _ a) = a

-- Assumptions FIXME: branch lengths -- we could have multiple set of them.
--                                      what we really need is transition probabilities for each partition.

data IndelModel = IndelModel
data ATModel = ATModel [MixtureModels] [IndelModel] [Double] [Double]
smodels        (ATModel ss _ _ _) = ss
imodels        (ATModel _ is _ _) = is
scales         (ATModel _ _ rs _) = rs
branch_lengths (ATModel _ _ _ ls) = ls

