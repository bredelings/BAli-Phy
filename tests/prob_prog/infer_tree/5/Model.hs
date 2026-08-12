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

    let multiFreqModel = multiFrequency tree nodeMap tn93Model
        gammaModel = always multiFreqModel +> gammaRatesOn alpha 4

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

main = do
    options <- execParser $
      info
        (modelRunOptions "Model" 200000
          (strArgument (metavar "ALIGNMENT" <> help "Aligned DNA sequences")) <**> helper)
        fullDesc
    runInfo <- initializeModelRun (testMode options) (outputName options)
    let nucs = dna

    seqData <- mkAlignedCharacterData nucs <$> loadSequences (modelInputs options)

    logTree <- case runInfo of
      TestRun -> return noLogger
      MCMCRun directory -> treeLogger (directory </> "C1.trees")

    context <- makeModelContext runInfo (logFormats options) $ model seqData nucs logTree

    case runInfo of
      TestRun -> printInitialModel (logFormats options) context
      MCMCRun directory -> do
        reportModelRun (iterations options) (logFormats options) directory
        runMCMC (iterations options) context

    verbosity <- getVerbosity
    if verbosity > 0 then writeTraceGraph context else return ()
