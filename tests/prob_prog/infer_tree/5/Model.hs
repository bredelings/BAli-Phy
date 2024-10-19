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

model seqData nucs = do

    let taxa = getTaxa seqData
        n = 3

    tree   <- sample $ uniformRootedTree taxa (gamma 0.5 (1/fromIntegral (length taxa)))
    scale <- sample $ gamma 0.5 2

    kappa1 <- sample $ logNormal 0 1
    kappa2 <- sample $ logNormal 0 1

    let tn93Model freqs = tn93' nucs kappa1 kappa2 freqs

    freqs  <- sample $ dirichletMixture n 1 $ symmetricDirichletOn (letters nucs) 1
    nodeMap <- sample $ iidMap (getNodesSet tree) freqs
    let nodeInfo n = nodeMap IntMap.! n
        nodeProp freqs = list_to_vector $ frequencies_from_dict nucs freqs
        edgeProp freqs = tn93Model freqs
        smap = list_to_vector [0..3]
        rate = 1
        multiFreqModel = MultiFrequency nucs smap rate nodeInfo nodeProp edgeProp

    observe seqData $ phyloCTMC tree (alignmentLength seqData) multiFreqModel scale

    let tlength = treeLength tree
        substs = parsimony tree (unitCostMatrix nucs) seqData

    return ["tree" %=% writeNewick tree,
--            "nodeMap" %=% nodeMap,
            "scale" %=% scale,
            "scale*|T|" %=% scale * tlength,
            "#substs" %=% substs,
            "freqs" %=% freqs,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2,
            "tn93:frequencies" %=% freqs]

main logDir = do
    [filename] <- getArgs

    let nucs = dna

    seqData <- mkAlignedCharacterData nucs <$> load_sequences filename

    return $ model seqData nucs

