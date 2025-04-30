module Model where

import           Probability
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           System.Environment  -- for getArgs

model seqData = do

    let taxa = zip [0..] $ getTaxa seqData

    age    <- sample $ gamma 0.5 2
    tree   <- addLabels taxa <$> sample (uniformTimeTree age (length taxa))

    freqs  <- sample $ symmetricDirichletOn ["A", "C", "G", "T"] 1
    kappa1 <- sample $ logNormal 0 1
    kappa2 <- sample $ logNormal 0 1

    let tn93_model = tn93' dna kappa1 kappa2 freqs

    observe seqData $ phyloCTMC tree (alignmentLength seqData) tn93_model 1

    return ["tree" %=% writeNewick tree,
            "age" %=% age,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2,
            "tn93:frequencies" %=% freqs]

main logDir = do
    [filename] <- getArgs

    seqData <- mkAlignedCharacterData dna <$> loadSequences filename

    return $ model seqData

