module Model where

import           Probability
import           Probability.Logger
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           MCMC (scaleGroupSlice)
import           Tree
import           Tree.Newick
import           SModel
import           System.Environment (getArgs)
import           System.FilePath

{- Problems:

 1. We can explain the data ignoring the sample ages if we have high low mutation rates and deep coalescence times.
    - We want the prior to favor high mutation rates.
    - We want the prior to favor a recent root age => high coalescent rates => small population sizes.
 3. N/tau prints as tau

-}

smodel_prior nucleotides =  do
    freqs  <- sample $ symmetricDirichletOn (letters nucleotides) 1
    kappa1 <- sample $ logNormal (log 2) (1/4)
    kappa2 <- sample $ logNormal (log 2) (1/4)

    let tn93_model = tn93' nucleotides kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)


tree_prior taxa = do

    popSize <- sample $ logNormal 3 2  -- Favor small population sizes -> high coalescent rates -> more recent root age.

    let taxonAges = getTaxonAges taxa "s(\\d+)$" Forward
        popSizes = [(0, popSize)]

    tree <- sample $ coalescentTree taxonAges popSizes

    let loggers   = ["N_over_tau" %=% popSize,
                     "coalescentPr" %=% ln (density (coalescentTree taxonAges popSizes) tree)]

    return (tree, loggers)


model seqData@[seqData1,seqData2,seqData3] logTree = do

    let taxa = getTaxa seqData1

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, sloggers    ) <- smodel_prior dna

    mu <- sample $ logNormal (-5) 1.25  -- Favor high mutation rates -> more recent root age.

    -- We can't inverse-scale mu because it is exp(modifiable), not directly modifiable.
    addMove 1 $ scaleGroupSlice [ nodeTime tree node | node <- internalNodes tree ]

    weights <- map (3*) <$> (sample $ symmetricDirichlet 3 3)

    let [mu1,mu2,mu3] = map (mu*) weights

    observe seqData1 $ phyloCTMC tree (alignmentLength seqData1) smodel mu1

    observe seqData2 $ phyloCTMC tree (alignmentLength seqData2) smodel mu2

    observe seqData3 $ phyloCTMC tree (alignmentLength seqData3) smodel mu3

    addLogger $ logTree (addInternalLabels tree)

    let tlength = treeLength tree
        substs = map (parsimony tree (unitCostMatrix dna)) seqData
        rootAge = nodeTime tree (root tree)
        loggers = ["tree" %>% tree_loggers,
                   "tn93" %>% sloggers,
                   "|T|" %=% tlength,
                   "mu*|T|" %=% tlength * mu,
                   "rootAge" %=% rootAge,
                   "weights" %=% weights,
                   "mu" %=% mu,
                   "mu1" %=% mu1,
                   "mu2" %=% mu2,
                   "mu3" %=% mu3,
                   "#substs" %=% substs,
                   "#total_substs" %=% sum substs]

    return loggers

main logDir = do

    [filename] <- getArgs

    sequences <- load_sequences filename

    let seqData = [ mkAlignedCharacterData dna $ select_range range sequences | range <- ["3-.\\3", "1-.\\3", "2-.\\3"]]

    -- This is placed in the wrong directory.
    logTree <- treeLogger $ logDir </> "C1.trees"

    return $ model seqData logTree

