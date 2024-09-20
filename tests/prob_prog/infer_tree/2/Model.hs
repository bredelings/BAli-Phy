module Model where

import           Probability
import           Bio.Alphabet  (dna)
import           Bio.Alignment (alignmentLength)
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           System.Environment  -- for getArgs

branch_length_dist topology b = gamma 0.5 (2 / fromIntegral n) where n = numBranches topology

model seqData = do

    let taxa = getTaxa seqData

    scale  <- prior $ gamma 0.5 2

    tree   <- prior $ uniformLabelledTree'' taxa branch_length_dist

    freqs  <- prior $ symmetricDirichletOn ["A", "C", "G", "T"] 1
    kappa1 <- prior $ logNormal 0 1
    kappa2 <- prior $ logNormal 0 1

    let tn93_model = tn93' dna kappa1 kappa2 freqs

    observe seqData $ phyloCTMC tree (alignmentLength seqData) tn93_model scale

    return ["tree" %=% write_newick tree,
            "scale" %=% scale,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2,
            "tn93:frequencies" %=% freqs,
            "|T|" %=% treeLength tree,
            "scale*|T|" %=% scale * treeLength tree]

main = do
    [filename] <- getArgs

    seqData <- mkAlignedCharacterData dna <$> load_sequences filename

    return $ model seqData
