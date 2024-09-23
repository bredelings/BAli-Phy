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

{- Problems:

 1. The population size has these jumps to really large values. (So 1/N just to really small values).
 2. The large jumps seem not to affect the prior.
 3. N/tau prints as tau
 4. There's bad mixing between mu and |T| -- how to handle this?
    - We could try to scale the node times, but we don't want to move the tip nodes...

-}

smodel_prior nucleotides =  do
    freqs  <- sample $ symmetricDirichletOn (letters nucleotides) 1
    kappa1 <- sample $ logNormal (log 2) (1/4)
    kappa2 <- sample $ logNormal (log 2) (1/4)

    let tn93_model = tn93' nucleotides kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)


tree_prior taxa = do

    popSize <- sample $ logLaplace 2 2

    let taxonAges = getTaxonAges taxa "s(\\d+)$" Forward
        rateShifts = [(0, popSize)]

    tree <- sample $ coalescentTree taxonAges rateShifts

    let loggers   = ["N_over_tau" %=% popSize]

    return (tree, loggers)


model seqData logTree = do

    let taxa = getTaxa seqData

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, sloggers    ) <- smodel_prior dna

    mu <- sample $ logLaplace (-5) 1

    let tlength = treeLength tree
        substs = parsimony tree seqData (unitCostMatrix dna)
        rootAge = nodeTime tree (root tree)
        loggers = ["tree" %>% tree_loggers,
                   "tn93" %>% sloggers,
                   "|T|" %=% tlength,
                   "mu*|T|" %=% tlength * mu,
                   "rootAge" %=% rootAge,
                   "mu" %=% mu,
                   "#substs" %=% substs]

    observe seqData $ phyloCTMC tree (alignmentLength seqData) smodel mu

    addLogger $ logTree (addInternalLabels tree)

    return loggers

main = do
    [filename] <- getArgs

    seqData <- mkAlignedCharacterData dna <$> load_sequences filename

    logTree <- treeLogger "C1.trees"

    return $ model seqData logTree

