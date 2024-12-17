module Model where

import           Probability
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           System.Environment  -- for getArgs
import qualified Data.IntMap as IntMap
import           Probability.Logger
import           System.FilePath ( (</>) )

model seqData nucs logTree = do

    let taxa = getTaxa seqData

    n <- (min 10) <$> (1+) <$> sample (geometric 0.5)

    tree   <- sample $ uniformRootedTree taxa (gamma 0.5 (1/fromIntegral (length taxa)))
    scale <- sample $ gamma 0.5 2

    kappa1 <- sample $ logNormal 0 1
    kappa2 <- sample $ logNormal 0 1

    let tn93Model freqs = tn93' nucs kappa1 kappa2 freqs

    freqs  <- sample $ dirichletMixture n 2 $ symmetricDirichletOn (getLetters nucs) 1
    nodeMap <- sample $ iidMap (getNodesSet tree) freqs
    let multiFreqModel = multiFrequency nucs (simpleSMap nucs) nodeMap (frequencies_from_dict nucs) tn93Model
        freqs3 = sortDist freqs

    observe seqData $ phyloCTMC tree (alignmentLength seqData) multiFreqModel scale

    let tlength = treeLength tree
        substs = parsimony tree (unitCostMatrix nucs) seqData

    addLogger $ logTree $ addInternalLabels $ scaleBranchLengths scale $ tree

    return ["nFreqs" %=% n,
            "scale" %=% scale,
            "scale*|T|" %=% scale * tlength,
            "#substs" %=% substs,
            "freqs" %=% freqs3,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2]

main logDir = do
    [filename] <- getArgs

    let nucs = dna

    seqData <- mkAlignedCharacterData nucs <$> load_sequences filename

    logTree <- treeLogger (logDir </> "C1.trees")

    return $ model seqData nucs logTree

