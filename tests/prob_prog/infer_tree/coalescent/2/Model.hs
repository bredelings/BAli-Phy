module Model where

import           Probability
import           Probability.Logger
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel
import           System.Environment  -- for getArgs

smodel_prior nucleotides =  do
    freqs  <- sample $ symmetricDirichletOn (letters nucleotides) 1
    kappa1 <- sample $ logNormal (log 2) (1/4)
    kappa2 <- sample $ logNormal (log 2) (1/4)

    let tn93_model = tn93' nucleotides kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)


tree_prior taxa = do

    theta <- sample $ logLaplace 2 2

    let taxonAges = getTaxonAges taxa "s(\\d+)$" Forward
        rateShifts = [(0, theta)]

    tree <- sample $ coalescentTree taxonAges rateShifts

    let loggers   = ["theta" %=% theta, "|T|" %=% treeLength tree]

    return (tree, loggers)


model seqData logTree = do

    let taxa = getTaxa seqData

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, sloggers    ) <- smodel_prior dna

    mu <- sample $ logLaplace (-4) 1

    let loggers = tree_loggers ++ ["tn93" %>% sloggers,
                                   "mu" %=% mu,
                                   "#substs" %=% parsimony tree seqData (unitCostMatrix dna)]

    observe seqData $ phyloCTMC tree (alignmentLength seqData) smodel mu

    addLogger $ logTree (addInternalLabels tree)

    return loggers

main = do
    [filename] <- getArgs

    seqData <- mkAlignedCharacterData dna <$> load_sequences filename

    logTree <- treeLogger "C1.trees"

    return $ model seqData logTree

