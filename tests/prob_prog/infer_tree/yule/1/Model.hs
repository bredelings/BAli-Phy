module Model where

import           BAliPhy.Run
import           Probability
import           Probability.Logger
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           MCMC (runMCMC, scaleGroupSlice)
import           Options.Applicative
import           Probability.Random (writeTraceGraph)
import           Tree
import           Tree.Newick
import           SModel
import           System.FilePath

smodel_prior nucleotides =  do
    freqs  <- sample $ symmetricDirichletOn (getLetters nucleotides) 1
    kappa1 <- sample $ logNormal (log 2) (1/4)
    kappa2 <- sample $ logNormal (log 2) (1/4)

    let tn93_model = tn93' nucleotides kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)


tree_prior taxa = do

    lambda <- sample $ logLaplace 0 2

    tree <- sample $ yule taxa lambda

    let loggers   = ["lambda" %=% lambda]

    return (tree, loggers)


model seqData logTree = do

    let taxa = getTaxa seqData

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, sloggers    ) <- smodel_prior dna

    mu <- sample $ logLaplace (-5) 1

    -- We can't inverse-scale mu because it is exp(modifiable), not directly modifiable.
    addMove 1 $ scaleGroupSlice [ nodeTime tree node | node <- internalNodes tree ]

    let tlength = treeLength tree
        substs = parsimony tree (unitCostMatrix dna) seqData
        rootAge = nodeTime tree (root tree)
        loggers = ["tree" %>% tree_loggers,
                   "tn93" %>% sloggers,
                   "|T|" %=% tlength,
                   "mu*|T|" %=% tlength * mu,
                   "mu" %=% mu,
                   "rootAge" %=% rootAge,
                   "#substs" %=% substs]

    observe seqData $ phyloCTMC tree (alignmentLength seqData) smodel mu

    addLogger $ logTree (addInternalLabels tree)

    return loggers

main = do
    options <- execParser $
      info
        (modelRunOptions "Model" 200000
          (strArgument (metavar "ALIGNMENT" <> help "Aligned sequence file")) <**> helper)
        (fullDesc <> progDesc "Run the Yule-tree example")

    run <- prepareModelRun (testMode options) (outputName options)

    seqData <- mkAlignedCharacterData dna <$> loadSequences (modelInputs options)

    logTree <- case run of
      TestRun -> return noLogger
      MCMCRun directory -> treeLogger $ directory </> "C1.trees"

    context <- makeModelContext run (logFormats options) $ model seqData logTree

    case run of
      TestRun -> printInitialModel (logFormats options) context
      MCMCRun directory -> do
        reportModelRun (iterations options) (logFormats options) directory
        runMCMC (iterations options) context

    verbosity <- getVerbosity
    if verbosity > 0 then writeTraceGraph context else return ()
