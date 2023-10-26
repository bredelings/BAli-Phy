module Model where

import           Probability
import           Bio.Alphabet  (dna)
import           Bio.Alignment (alignmentLength)
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           Probability.Distribution.OnTree
import           System.Environment  -- for getArgs

branch_length_dist topology b = gamma 0.5 (2 / fromIntegral n) where n = numBranches topology

model seqData = do

    let taxa = getTaxa seqData

    scale1  <- prior $ gamma 0.5 2

    tree   <- prior $ uniform_labelled_tree taxa branch_length_dist
    let tree1 = scale_branch_lengths scale1 tree

    freqs  <- prior $ symmetric_dirichlet_on ["A", "C", "G", "T"] 1
    kappa1 <- prior $ log_normal 0 1
    kappa2 <- prior $ log_normal 0 1

    let tn93_model = tn93' dna kappa1 kappa2 freqs

    observe seqData $ ctmc_on_tree tree1 (alignmentLength seqData) tn93_model

    return ["tree1" %=% write_newick tree1,
            "scale" %=% scale1,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2,
            "tn93:frequencies" %=% freqs,
            "|T|" %=% tree_length tree,
            "scale1*|T|" %=% tree_length tree1]

main = do
    [filename] <- getArgs

    seqData <- mkAlignedCharacterData dna <$> load_sequences filename

    return $ model seqData
