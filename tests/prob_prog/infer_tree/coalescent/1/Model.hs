module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel

smodel_prior nucleotides =  do
    freqs  <- sample $ symmetricDirichletOn (getLetters nucleotides) 1
    kappa1 <- sample $ logNormal (log 2) (1/4)
    kappa2 <- sample $ logNormal (log 2) (1/4)

    let tn93_model = tn93' nucleotides kappa1 kappa2 freqs
    let loggers    = ["kappa1" %=% kappa1, "kappa2" %=% kappa2, "frequencies" %=% freqs]

    return (tn93_model, loggers)


tree_prior taxa = do

    theta <- sample $ logLaplace (-5) 2

    let taxonAges = [(taxon, 0.0) | taxon <- taxa]
        rateShifts = [(0.0, theta)]

    tree <- sample (coalescentTree taxonAges rateShifts)

    let loggers   = ["tree" %=% writeNewick tree, "theta" %=% theta]
    return (tree, loggers)


model seqData = do
    let taxa = getTaxa seqData

    (tree  , tree_loggers) <- tree_prior taxa

    (smodel, sloggers    ) <- smodel_prior dna

    let loggers = tree_loggers ++ ["tn93" %>% sloggers]

    observe seqData $ phyloCTMC tree (alignmentLength seqData) smodel 1

    return loggers

main = do
    (options, filename) <- execParser $
      modelRunParserWith "Model" 200000 $
        strArgument (metavar "ALIGNMENT" <> help "Aligned DNA sequences")
    runInfo <- initializeModelRun (testMode options) (outputName options)

    seqData <- mkAlignedCharacterData dna <$> loadSequences filename
    context <- makeModelContext runInfo (logFormats options) $ model seqData

    case runInfo of
      TestRun -> printInitialModel (logFormats options) context
      MCMCRun directory -> do
        reportModelRun (iterations options) (logFormats options) directory
        runMCMC (iterations options) context

    verbosity <- getVerbosity
    if verbosity > 0 then writeTraceGraph context else return ()
