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

    let n = 2

    tree   <- sample $ uniformRootedTree taxa (gamma 0.5 (1/fromIntegral (length taxa)))
    scale <- sample $ gamma 0.5 2

    kappa1 <- sample $ logNormal 0 1
    kappa2 <- sample $ logNormal 0 1

    let tn93Model freqs = tn93' nucs kappa1 kappa2 freqs

    freqs  <- sample $ dirichletMixture n 2 $ symmetricDirichletOn (getLetters nucs) 1
    nodeMap <- sample $ iidMap (getNodesSet tree) freqs
    alpha <- sample $ logLaplace 6 2

    let multiFreqModel = multiFrequency' tree nodeMap id tn93Model
        gammaModel = always multiFreqModel +> gammaRates alpha 4

    observe seqData $ phyloCTMC tree (alignmentLength seqData) gammaModel scale

    let tlength = treeLength tree
        substs = parsimony tree (unitCostMatrix nucs) seqData

    addLogger $ logTree $ addInternalLabels $ scaleBranchLengths scale $ tree

    return ["nFreqs" %=% n,
            "scale" %=% scale,
            "scale*|T|" %=% scale * tlength,
            "#substs" %=% substs,
            "freqs" %=% sortDist freqs,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2,
            "gamma:alpha" %=% alpha]

main logDir = do
    [filename] <- getArgs

    let nucs = dna

    seqData <- mkAlignedCharacterData nucs <$> load_sequences filename

    logTree <- treeLogger (logDir </> "C1.trees")

    return $ model seqData nucs logTree

